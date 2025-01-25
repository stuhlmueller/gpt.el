#!/usr/bin/env python

# Copyright (C) 2022 Andreas Stuhlmueller, 2024 Anselm Coogan
# License: MIT
# SPDX-License-Identifier: MIT

import sys
import argparse
import re
from pathlib import Path
from enum import Enum
from typing import Union, AnyStr, Iterator

try:
    import openai
except ImportError:
    openai = None
try:
    import anthropic
except ImportError:
    anthropic = None
try:
    import jsonlines
except ImportError:
    jsonlines = None

DEEPSEEK_URL = "https://api.deepseek.com"


class APIType(str, Enum):
    openai = "openai"
    anthropic = "anthropic"
    deepseek = "deepseek"


def _ensure_api_type_is_available(api_type: APIType) -> None:
    if api_type == APIType.openai and openai is None:
        print("Error: OpenAI Python package is not installed.")
        print("Please install by running `pip install openai'.")
        sys.exit(1)
    elif api_type == APIType.anthropic and anthropic is None:
        print("Error: Anthropic Python package is not installed.")
        print("Please install by running `pip install anthropic'.")
        sys.exit(1)
    elif api_type == APIType.deepseek and openai is None:
        print("Error: OpenAI Python package is not installed and needed for deepseek.")
        print("Please install by running `pip install openai'.")
        sys.exit(1)


def _ensure_api_key_is_set(api_key: str, api_type: APIType) -> None:
    if api_key == "NOT SET":
        api_name = {
            APIType.openai: "OpenAI",
            APIType.anthropic: "Anthropic",
            APIType.deepseek: "Deepseek",
        }[api_type]
        key_var = {
            APIType.openai: "gpt-openai-key",
            APIType.anthropic: "gpt-anthropic-key",
            APIType.deepseek: "gpt-deepseek-key",
        }[api_type]
        print(f"Error: {api_name} API key not set.")
        print(f'Add (setq {key_var} "sk-...") to your Emacs init.el file.')
        sys.exit(1)


def _build_messages_for_openai(
    matches: Iterator[re.Match[AnyStr]], instructions: str | None
) -> list[dict[str, str]]:
    messages = [{"role": "system", "content": "You are a helpful assistant."}]
    if instructions:
        messages.append({"role": "system", "content": instructions})
    for m in matches:
        role = str(m.group(1).lower())
        content = str(m.group(2).strip())
        if content:
            messages.append({"role": role, "content": content})
    return messages


def _build_messages_for_anthropic(
    matches: Iterator[re.Match[AnyStr]],
) -> list[dict[str, str]]:
    class AnthropicRole(str, Enum):
        user = "user"
        assistant = "assistant"

    # Anthropic requires alternating user and assistant messages,
    # so we group user messages together
    messages = []
    current_user_message = ""
    for m in matches:
        role = AnthropicRole(m.group(1).lower()).value
        content = str(m.group(2).strip())
        if not content:
            continue
        if role == AnthropicRole.user:
            current_user_message += "\n\n" + content
        else:
            if current_user_message:
                messages.append(
                    {"role": AnthropicRole.user.value, "content": current_user_message}
                )
                current_user_message = ""
            messages.append({"role": role, "content": content})
    if current_user_message:
        messages.append(
            {"role": AnthropicRole.user.value, "content": current_user_message}
        )
    return messages


def _setup_client(
    api_key: str, api_type: APIType
) -> openai.OpenAI | anthropic.Anthropic:
    match api_type:
        case APIType.openai:
            return openai.OpenAI(api_key=api_key)
        case APIType.deepseek:
            return openai.OpenAI(api_key=api_key, base_url=DEEPSEEK_URL)
        case APIType.anthropic:
            return anthropic.Anthropic(api_key=api_key)


def _stream_chat_completions(
    prompt: str,
    api_key: str,
    api_type: APIType,
    model: str,
    max_tokens: int,
    temperature: float,
    instructions: str | None,
) -> Union[openai.Stream, anthropic.Anthropic]:
    """Stream chat completions from the specified API."""
    _ensure_api_type_is_available(api_type)
    _ensure_api_key_is_set(api_key, api_type)

    client = _setup_client(api_key, api_type)

    pattern = re.compile(
        r"^(User|Assistant):(.+?)(?=\n(?:User|Assistant):|\Z)", re.MULTILINE | re.DOTALL
    )
    matches = pattern.finditer(prompt)

    if api_type in (APIType.openai, APIType.deepseek):
        messages = _build_messages_for_openai(matches, instructions)
    else:
        messages = _build_messages_for_anthropic(matches)
    try:
        if api_type in (APIType.openai, APIType.deepseek):
            return client.chat.completions.create(
                model=model,
                messages=messages,
                max_tokens=max_tokens,
                temperature=temperature,
                stream=True,
            )
        else:
            # for openai the instructions are in the messages
            extra_kwargs = {"system": instructions} if instructions else {}
            return client.messages.create(
                model=model,
                messages=messages,
                max_tokens=max_tokens,
                temperature=temperature,
                stream=True,
                **extra_kwargs,
            )
    except (openai.APIError, anthropic.APIError) as error:
        print(f"Error: {error}")
        sys.exit(1)


def _print_and_collect_completions(stream, api_type: APIType) -> str:
    """Print and collect completions from the stream."""
    completion_text = ""
    if api_type in (APIType.openai, APIType.deepseek):
        for chunk in stream:
            if chunk.choices[0].delta.content:
                text = chunk.choices[0].delta.content
                print(text, end="", flush=True)
                completion_text += text
    elif api_type == APIType.anthropic:
        for chunk in stream:
            if chunk.type == "content_block_delta":
                text = chunk.delta.text
                print(text, end="", flush=True)
                completion_text += text
    else:
        raise ValueError(f"Unsupported API type '{api_type}'")

    return completion_text


def _write_to_jsonl(prompt: str, completion: str, path: Path) -> None:
    """Write the prompt and completion to a jsonl file."""
    # TODO @anselm: consider removing this
    if jsonlines is None:
        return
    path.touch(exist_ok=True)
    try:
        with jsonlines.open(path, mode="a") as writer:
            writer.write({"prompt": prompt, "completion": completion})
    except IOError as error:
        print(f"Error: {error}")
        sys.exit(1)


def _stream_chat(
    prompt: str,
    api_key: str,
    api_type: APIType,
    model: str,
    max_tokens: int,
    temperature: float,
) -> None:
    instruction_sep = "GPTInstructions: "
    if instruction_sep in prompt:
        prompt, instructions = prompt.split(instruction_sep)
    else:
        instructions = None
    stream = _stream_chat_completions(
        prompt, api_key, api_type, model, max_tokens, temperature, instructions
    )
    completion_text = _print_and_collect_completions(stream, api_type)
    file_name = Path.home() / ".emacs_prompts_completions.jsonl"
    _write_to_jsonl(prompt, completion_text, file_name)


def _parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "prompt_file", type=Path, help="The file that contains the prompt."
    )
    parser.add_argument("api_key", help="The API key to use for the selected API.")
    parser.add_argument(
        "model", help="The model to use (e.g., 'gpt-4', 'claude-3-sonnet-20240229')."
    )
    parser.add_argument(
        "max_tokens", help="Max tokens value to be used with the API.", type=int
    )
    parser.add_argument(
        "temperature", help="Temperature value to be used with the API.", type=float
    )
    parser.add_argument(
        "api_type",
        type=APIType,
        choices=list(APIType),
        help="Which GPT provider to use.",
    )
    return parser.parse_args()


if __name__ == "__main__":
    args = _parse_args()
    with args.prompt_file.open("r", encoding="utf-8") as fdes:
        prompt = fdes.read()
    _stream_chat(
        prompt,
        args.api_key,
        args.api_type,
        args.model,
        args.max_tokens,
        args.temperature,
    )
