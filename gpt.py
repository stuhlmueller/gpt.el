#!/usr/bin/env python

# Copyright (C) 2022 Andreas Stuhlmueller
# License: MIT
# SPDX-License-Identifier: MIT

import sys
import os
import argparse
import re
from pathlib import Path
from typing import (
    Literal,
    Union,
    TYPE_CHECKING,
    cast,
    Iterator,
    Optional,
    Any,
    Match,
)

if TYPE_CHECKING:
    from openai.types.chat import ChatCompletion as OpenAIChatCompletion
    from openai.types.chat.chat_completion_chunk import (
        ChatCompletionChunk as OpenAIChatCompletionChunk,
    )
    from openai import Stream as OpenAIStream
    from anthropic.types import MessageStreamEvent as AnthropicMessageStreamEvent
    from google.genai.types import GenerateContentResponse

APIType = Literal["openai", "anthropic", "google"]
CompletionStream = Union[
    "OpenAIStream[OpenAIChatCompletionChunk]",  # OpenAI streaming response
    "OpenAIChatCompletion",  # OpenAI non-streaming response (o1/o3)
    Iterator["AnthropicMessageStreamEvent"],  # Anthropic streaming response
    Iterator["GenerateContentResponse"],  # Google streaming response
]

# These modules are imported conditionally to handle missing dependencies gracefully
openai: Optional[Any] = None
anthropic: Optional[Any] = None
genai: Optional[Any] = None
jsonlines: Optional[Any] = None

try:
    import openai
except ImportError:
    pass

try:
    import anthropic
except ImportError:
    pass

try:
    import google.genai as genai
    from google.genai import types as genai_types
except ImportError:
    pass

try:
    import jsonlines
except ImportError:
    pass


def parse_args() -> argparse.Namespace:
    """Parse command line arguments.

    Returns:
        Namespace containing the parsed command line arguments
    """
    parser = argparse.ArgumentParser(
        description="Generate text completions using OpenAI, Anthropic, or Google APIs",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument("api_key", help="The API key to use for the selected API.")
    parser.add_argument(
        "model", help="The model to use (e.g., gpt-4o, claude-3-sonnet-latest, google-2.5.pro-exp-03-25)"
    )
    parser.add_argument(
        "max_tokens",
        help="Max tokens value to be used with the API.",
        type=str,  # Will be converted to int later
    )
    parser.add_argument(
        "temperature",
        help="Temperature value to be used with the API.",
        type=str,  # Will be converted to float later
    )
    parser.add_argument(
        "api_type",
        type=str,
        choices=("openai", "anthropic", "google"),
        help="The type of API to use: 'openai', 'anthropic', or 'google'.",
    )
    parser.add_argument("prompt_file", help="The file that contains the prompt.")
    return parser.parse_args()


def read_input_text() -> str:
    """Read input text from stdin.

    Returns:
        The text read from standard input
    """
    return sys.stdin.read()


def stream_openai_chat_completions(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: str,
    temperature: str,
) -> Union["OpenAIStream[OpenAIChatCompletionChunk]", "OpenAIChatCompletion"]:
    """Stream chat completions from the OpenAI API.

    Args:
        prompt: The prompt text with User/Assistant conversation format
        api_key: OpenAI API key
        model: Model identifier (e.g., 'gpt-4o', 'o1', 'o3')
        max_tokens: Maximum tokens to generate (as string, will be converted to int)
        temperature: Sampling temperature (as string, will be converted to float)

    Returns:
        Either a streaming or non-streaming completion response

    Raises:
        SystemExit: On configuration or API errors
    """
    if openai is None:
        print("Error: OpenAI Python package is not installed.")
        print("Please install by running `pip install openai'.")
        sys.exit(1)

    if api_key == "NOT SET":
        print("Error: OpenAI API key not set.")
        print(
            'Add (setq gpt-openai-key "sk-Aes.....AV8qzL") to your Emacs init.el file.'
        )
        sys.exit(1)

    client = openai.OpenAI(api_key=api_key)

    messages: list[dict[str, str]] = [
        {"role": "system", "content": "You are a helpful assistant."}
    ]
    pattern = re.compile(
        r"^(User|Assistant):(.+?)(?=\n(?:User|Assistant):|\Z)", re.MULTILINE | re.DOTALL
    )
    matches: Iterator[Match[str]] = pattern.finditer(prompt)
    for match in matches:
        role = match.group(1).lower()
        content = match.group(2).strip()
        messages.append({"role": role, "content": content})

    params: dict[str, Any] = {
        "model": model,
        "messages": messages,
    }
    if model not in {"o1", "o3", "o3-mini"}:
        params.update(
            {
                "max_tokens": int(max_tokens),
                "temperature": float(temperature),
                "stream": True,
            }
        )

    try:
        return client.chat.completions.create(**params)
    except openai.APIError as error:
        print(f"Error: {error}")
        sys.exit(1)
    except Exception as error:
        print(f"Unexpected error: {error}")
        sys.exit(1)


def stream_anthropic_chat_completions(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: str,
    temperature: str,
) -> Iterator["AnthropicMessageStreamEvent"]:
    """Stream chat completions from the Anthropic API.

    Args:
        prompt: The prompt text with User/Assistant conversation format
        api_key: Anthropic API key
        model: Model identifier (e.g., 'claude-3-sonnet-latest')
        max_tokens: Maximum tokens to generate (as string, will be converted to int)
        temperature: Sampling temperature (as string, will be converted to float)

    Returns:
        Iterator of message stream events from the Anthropic API

    Raises:
        SystemExit: On configuration or API errors
    """
    if anthropic is None:
        print("Error: Anthropic Python package is not installed.")
        print("Please install by running `pip install anthropic'.")
        sys.exit(1)

    if api_key == "NOT SET":
        print("Error: Anthropic API key not set.")
        print(
            'Add (setq gpt-anthropic-key "sk-ant-api03-...") to your Emacs init.el file.'
        )
        sys.exit(1)

    client = anthropic.Anthropic(api_key=api_key)

    messages: list[dict[str, str]] = []
    pattern = re.compile(
        r"^(User|Assistant):(.+?)(?=\n(?:User|Assistant):|\Z)", re.MULTILINE | re.DOTALL
    )
    matches: Iterator[Match[str]] = pattern.finditer(prompt)

    # Anthropic requires alternating user and assistant messages,
    # so we group user messages together
    current_user_message: Optional[str] = None

    for match in matches:
        role = "user" if match.group(1).lower() == "user" else "assistant"
        content = match.group(2).strip()

        if role == "user":
            if current_user_message is None:
                current_user_message = content
            else:
                current_user_message += "\n\n" + content
        else:
            if current_user_message is not None:
                messages.append({"role": "user", "content": current_user_message})
                current_user_message = None
            messages.append({"role": "assistant", "content": content})

    if current_user_message is not None:
        messages.append({"role": "user", "content": current_user_message})

    try:
        return client.messages.create(
            model=model,
            messages=messages,
            max_tokens=int(max_tokens),
            temperature=float(temperature),
            stream=True,
        )
    except anthropic.APIError as error:
        print(f"Error: {error}")
        sys.exit(1)
    except Exception as error:
        print(f"Unexpected error: {error}")
        sys.exit(1)


def stream_google_chat_completions(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: str,
    temperature: str,
) -> Iterator[Any]:
    """Stream chat completions from the Google Gemini API.

    Args:
        prompt: The prompt text with User/Assistant conversation format
        api_key: Google API key
        model: Model identifier (e.g., 'gemini-2.5-pro-exp-03-25')
        max_tokens: Maximum tokens to generate (as string, will be converted to int)
        temperature: Sampling temperature (as string, will be converted to float)

    Returns:
        Iterator of generation chunks from the Google Gemini API

    Raises:
        SystemExit: On configuration or API errors
    """
    if genai is None:
        print("Error: Google Generative AI Python package is not installed.")
        print("Please install by running `pip install google-genai'.")
        sys.exit(1)

    if api_key == "NOT SET":
        print("Error: Google API key not set.")
        print(
            'Add (setq gpt-google-key "YOUR_API_KEY") to your Emacs init.el file.'
        )
        sys.exit(1)

    # Create Google client with API key
    client = genai.Client(api_key=api_key)

    # Parse the prompt to extract the conversation
    contents = []
    
    # Check if this is a conversation or a simple prompt
    if "User:" in prompt or "Human:" in prompt or "Assistant:" in prompt or "Model:" in prompt:
        # Parse conversation with roles
        pattern = re.compile(
            r"^(User|Human|Assistant|Model):(.+?)(?=\n(?:User|Human|Assistant|Model):|\Z)", 
            re.MULTILINE | re.DOTALL
        )
        matches = list(pattern.finditer(prompt))
        
        for match in matches:
            role_text = match.group(1).lower()
            message_text = match.group(2).strip()
            
            # Map the role (user/human -> user, assistant/model -> model)
            if role_text in ["user", "human"]:
                # Create a user content
                content = genai_types.UserContent(
                    parts=[genai_types.Part.from_text(text=message_text)]
                )
            else:
                # Create a model content
                content = genai_types.ModelContent(
                    parts=[genai_types.Part.from_text(text=message_text)]
                )
            
            contents.append(content)
    else:
        # Simple prompt, just use the text directly
        # The SDK will convert it to a UserContent automatically
        contents = prompt

    try:
        # Use the client model API with the correct parameters
        response = client.models.generate_content_stream(
            model=model,
            contents=contents,
            config=genai_types.GenerateContentConfig(
                # Increase max_output_tokens to ensure sufficient generation space
                max_output_tokens=max(32000, int(max_tokens)),
                temperature=float(temperature),
            ),
        )
        
        return response
    except Exception as error:
        print(f"Error calling Google Gemini API: {error}")
        sys.exit(1)


def print_and_collect_completions(
    stream: CompletionStream,
    api_type: APIType,
) -> str:
    """Print and collect completions from the stream.

    Args:
        stream: The completion stream from either OpenAI, Anthropic, or Google API
        api_type: The type of API used ('openai', 'anthropic', or 'google')

    Returns:
        The complete text generated by the model
    """
    completion_text: str = ""

    if api_type == "openai":
        stream = cast(
            Union["OpenAIStream[OpenAIChatCompletionChunk]", "OpenAIChatCompletion"],
            stream,
        )
        if hasattr(stream, "choices"):  # Non-streaming response (o1/o3)
            stream = cast("OpenAIChatCompletion", stream)
            if stream.choices:
                text = stream.choices[0].message.content
                if text:
                    print(text, end="", flush=True)
                    completion_text = text
        else:  # Streaming response
            for chunk in cast("OpenAIStream[OpenAIChatCompletionChunk]", stream):
                if chunk.choices and chunk.choices[0].delta.content:
                    text = chunk.choices[0].delta.content
                    print(text, end="", flush=True)
                    completion_text += text
    elif api_type == "anthropic":
        stream = cast(Iterator["AnthropicMessageStreamEvent"], stream)
        current_completion = ""
        for chunk in stream:
            if chunk.type == "content_block_delta":
                delta = chunk.delta
                delta_text = getattr(delta, "text", None)
                if delta_text:
                    print(delta_text, end="", flush=True)
                    current_completion += delta_text
        return current_completion
    elif api_type == "google":
        # Use proper typing for Google API responses
        stream = cast(Iterator["GenerateContentResponse"], stream)
        completion_text = ""
        try:
            for chunk in stream:
                # Extract text directly using the text property as shown in example
                if hasattr(chunk, "text") and chunk.text is not None:
                    print(chunk.text, end="", flush=True)
                    completion_text += chunk.text
        except Exception as e:
            print(f"\nError processing Google Gemini response: {e}", file=sys.stderr)
            
        return completion_text
    else:
        # This should never happen due to validation in main()
        raise ValueError(f"Unsupported API type: {api_type}")

    # This return covers OpenAI case
    return completion_text


def write_to_jsonl(prompt: str, completion: str, path: Path) -> None:
    """Write the prompt and completion to a jsonl file.

    Args:
        prompt: The prompt text sent to the model
        completion: The completion text generated by the model
        path: The path to the output JSONL file

    Raises:
        SystemExit: On file I/O errors
    """
    if jsonlines is None:
        return

    # Create the file if it doesn't exist
    if not os.path.exists(path):
        try:
            with open(path, "w", encoding="utf-8"):
                pass  # Create the file
        except IOError as error:
            print(f"Error creating file: {error}")
            sys.exit(1)

    try:
        with jsonlines.open(path, mode="a") as writer:
            writer.write({"prompt": prompt, "completion": completion})
    except IOError as error:
        print(f"Error writing to file: {error}")
        sys.exit(1)
    except Exception as error:
        print(f"Unexpected error: {error}")
        sys.exit(1)


def main() -> None:
    """
    Main function to read a prompt from a file, generate completions
    using the specified API, and save the completions to a JSONL file.

    This function handles the overall workflow:
    1. Parse command line arguments
    2. Read the prompt from the specified file
    3. Generate completion using the appropriate API
    4. Process and collect the completion text
    5. Save the prompt and completion to a JSONL file
    """
    # Parse command-line arguments
    args: argparse.Namespace = parse_args()

    # Read prompt from file
    try:
        with open(args.prompt_file, "r", encoding="utf-8") as prompt_file:
            prompt: str = prompt_file.read()
    except IOError as error:
        print(f"Error reading prompt file: {error}")
        sys.exit(1)

    # Generate completion based on API type
    if args.api_type == "openai":
        stream = stream_openai_chat_completions(
            prompt, args.api_key, args.model, args.max_tokens, args.temperature
        )
    elif args.api_type == "anthropic":
        stream = stream_anthropic_chat_completions(
            prompt, args.api_key, args.model, args.max_tokens, args.temperature
        )
    elif args.api_type == "google":
        stream = stream_google_chat_completions(
            prompt, args.api_key, args.model, args.max_tokens, args.temperature
        )
    else:
        print(f"Error: Unsupported API type '{args.api_type}'")
        sys.exit(1)

    # Process and collect completion text
    completion_text: str = print_and_collect_completions(stream, args.api_type)

    # Save to JSONL file
    file_name: Path = Path.home() / ".emacs_prompts_completions.jsonl"
    write_to_jsonl(prompt, completion_text, file_name)


if __name__ == "__main__":
    main()
