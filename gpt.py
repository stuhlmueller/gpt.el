#!/usr/bin/env python
# Copyright (C) 2022‑2025 Andreas Stuhlmueller – MIT License
"""
gpt.py – Generate completions with OpenAI (Responses API + web‑search),
Anthropic (with extended thinking), or Google Gemini.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterator, Literal, Optional, Union, cast

# Third-party imports (optional)
openai: Optional[Any] = None
anthropic: Optional[Any] = None
genai: Optional[Any] = None
jsonlines: Optional[Any] = None

try:
    import openai  # type: ignore
except ImportError:
    pass

try:
    import anthropic  # type: ignore
except ImportError:
    pass

try:
    import google.genai as genai  # type: ignore
    from google.genai import types as genai_types  # type: ignore
except ImportError:
    pass

try:
    import jsonlines  # type: ignore
except ImportError:
    pass

# Type checking imports
if TYPE_CHECKING:  # pragma: no cover
    from anthropic.types import (
        MessageStreamEvent as AnthropicMessageStreamEvent,
    )
    from google.genai.types import GenerateContentResponse
    from openai import Stream as OpenAIStream

# Constants
DEFAULT_SYSTEM_MESSAGE = "You are a helpful assistant."
JSONL_OUTPUT_FILE = ".emacs_prompts_completions.jsonl"
DEFAULT_GOOGLE_MAX_TOKENS = 32000
DEFAULT_THINKING_BUDGET = 10000

APIType = Literal["openai", "anthropic", "google"]

CompletionStream = Union[
    OpenAIStream[Any],
    Iterator["AnthropicMessageStreamEvent"],
    Iterator["GenerateContentResponse"],
    Any
]


@dataclass
class Message:
    """Represents a chat message with role and content."""

    role: str
    content: str


class APIError(Exception):
    """Base exception for API-related errors."""

    pass


class MissingDependencyError(APIError):
    """Raised when a required package is not installed."""

    pass


class InvalidAPIKeyError(APIError):
    """Raised when an API key is invalid or not set."""

    pass


def parse_messages(
    prompt: str, supported_roles: set[str] | None = None
) -> list[Message]:
    """Parse a prompt string into a list of Message objects.

    Args:
        prompt: The prompt string containing role-based messages
        supported_roles: Set of supported roles (case-insensitive). If None, defaults to common roles.

    Returns:
        List of Message objects parsed from the prompt
    """
    if supported_roles is None:
        supported_roles = {"user", "assistant", "human", "model"}

    role_pattern = "|".join(supported_roles)
    pattern = re.compile(
        rf"^({role_pattern}):(.+?)(?=\n(?:{role_pattern}):|\Z)", re.S | re.M | re.I
    )

    messages: list[Message] = []
    for match in pattern.finditer(prompt):
        role = match.group(1).lower()
        content = match.group(2).strip()
        messages.append(Message(role=role, content=content))

    return messages


def check_dependency(module: Any, name: str, install_cmd: str) -> None:
    """Check if a required dependency is available and raise error if not."""
    if module is None:
        raise MissingDependencyError(f"{name} not installed – {install_cmd}")


def validate_api_key(api_key: str, api_name: str) -> None:
    """Validate that API key is properly set."""
    if api_key == "NOT SET" or not api_key.strip():
        raise InvalidAPIKeyError(f"{api_name} API key not set or invalid.")


def parse_args() -> argparse.Namespace:
    """Parse command-line arguments for the completion generator."""
    p = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description="Generate completions with OpenAI, Anthropic, or Google APIs.",
    )
    p.add_argument("api_key", help="API key for the selected provider")
    p.add_argument("model", help="Model name to use for completion")
    p.add_argument("max_tokens", type=int, help="Maximum tokens to generate")
    p.add_argument("temperature", type=float, help="Temperature for sampling (0.0-2.0)")
    p.add_argument("api_type", choices=("openai", "anthropic", "google"), help="API provider to use")
    p.add_argument("prompt_file", help="Path to file containing the prompt")
    p.add_argument(
        "--thinking-enabled",
        action="store_true",
        help="Enable extended thinking for Anthropic models",
    )
    p.add_argument(
        "--thinking-budget",
        type=int,
        default=DEFAULT_THINKING_BUDGET,
        help="Token budget for thinking",
    )
    p.add_argument(
        "--interleaved-thinking",
        action="store_true",
        help="Enable interleaved thinking with tools",
    )
    p.add_argument(
        "--web-search",
        action="store_true",
        help="Enable web search for supported models",
    )
    return p.parse_args()


def call_openai(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: int,
    temperature: float,
    web_search: bool = False,
) -> OpenAIStream[Any]:
    """Generate completions using OpenAI's Responses API.
    
    Args:
        prompt: The input prompt containing user/assistant messages or a raw string.
        api_key: OpenAI API key
        model: Model name (e.g., 'gpt-4', 'o4-mini', 'gpt-4.1')
        max_tokens: Maximum tokens to generate
        temperature: Sampling temperature (0.0-2.0)
        web_search: Whether to enable web search
        
    Returns:
        Stream of Responses API events
        
    Raises:
        MissingDependencyError: If openai package is not installed
        InvalidAPIKeyError: If API key is invalid
        APIError: For other API-related errors
    """
    check_dependency(openai, "OpenAI Python package", "`pip install -U openai`")
    validate_api_key(api_key, "OpenAI")

    assert openai is not None

    client = openai.OpenAI(api_key=api_key)

    try:
        parsed_messages = parse_messages(prompt, {"user", "assistant", "system"})
        
        api_input: list[dict[str, str]] | str
        if not parsed_messages:
            api_input = prompt
        else:
            final_input_messages: list[dict[str, str]] = []
            has_system_message = any(msg.role == "system" for msg in parsed_messages)

            if not has_system_message:
                final_input_messages.append({"role": "system", "content": DEFAULT_SYSTEM_MESSAGE})

            for msg in parsed_messages:
                final_input_messages.append({"role": msg.role, "content": msg.content})
            api_input = final_input_messages
        
        params: dict[str, Any] = {
            "model": model,
            "input": api_input,
            "stream": True,
            "max_output_tokens": max_tokens,
        }
        
        if web_search:
            params["tools"] = [{"type": "web_search_preview"}]
        
        if temperature != 0.0:
            params["temperature"] = temperature
        
        return client.responses.create(**params)

    except openai.AuthenticationError as e:
        raise InvalidAPIKeyError(f"OpenAI authentication failed: {e}") from e
    except openai.RateLimitError as e:
        raise APIError(f"OpenAI rate limit exceeded: {e}") from e
    except openai.APIConnectionError as e:
        raise APIError(f"OpenAI connection error: {e}") from e
    except openai.APIError as e:
        raise APIError(f"OpenAI API error: {e}") from e


def stream_anthropic(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: int,
    temperature: float,
    thinking_enabled: bool = False,
    thinking_budget: int = DEFAULT_THINKING_BUDGET,
    interleaved_thinking: bool = False,
    web_search: bool = False,
) -> Iterator["AnthropicMessageStreamEvent"]:
    """Generate completions using Anthropic's API with optional extended thinking.
    
    Args:
        prompt: The input prompt containing user/assistant messages
        api_key: Anthropic API key
        model: Model name (e.g., 'claude-3-opus-20240229')
        max_tokens: Maximum tokens to generate
        temperature: Sampling temperature (must be 1.0 if thinking is enabled)
        thinking_enabled: Whether to enable extended thinking mode
        thinking_budget: Token budget for thinking (must be less than max_tokens)
        interleaved_thinking: Whether to enable interleaved thinking with tools
        web_search: Whether to enable web search tool
        
    Returns:
        Iterator of Anthropic message stream events
        
    Raises:
        MissingDependencyError: If anthropic package is not installed
        InvalidAPIKeyError: If API key is invalid
        ValueError: If thinking parameters are invalid
        APIError: For other API-related errors
    """
    check_dependency(anthropic, "Anthropic Python package", "`pip install anthropic")
    validate_api_key(api_key, "Anthropic")

    assert anthropic is not None

    client = anthropic.Anthropic(api_key=api_key)

    parsed_messages = parse_messages(prompt, {"user", "assistant"})
    messages: list[dict[str, str]] = []

    current_user_content: Optional[str] = None

    for msg in parsed_messages:
        if msg.role == "user":
            if current_user_content:
                current_user_content += f"\n\n{msg.content}"
            else:
                current_user_content = msg.content
        else:
            if current_user_content:
                messages.append({"role": "user", "content": current_user_content})
                current_user_content = None
            messages.append({"role": "assistant", "content": msg.content})

    if current_user_content:
        messages.append({"role": "user", "content": current_user_content})

    try:
        if thinking_enabled and max_tokens <= thinking_budget:
            raise ValueError(
                f"max_tokens ({max_tokens}) must be greater than thinking_budget ({thinking_budget}) "
                "when thinking is enabled. Please increase max_tokens or decrease thinking_budget."
            )

        kwargs: dict[str, Any] = {
            "model": model,
            "messages": messages,
            "max_tokens": max_tokens,
            "temperature": (
                1 if thinking_enabled else temperature
            ),
            "stream": True,
        }

        if thinking_enabled:
            kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}

        headers: dict[str, str] = {}
        if interleaved_thinking:
            headers["anthropic-beta"] = "interleaved-thinking-2025-05-14"

        if headers:
            kwargs["extra_headers"] = headers

        if web_search:
            kwargs["tools"] = [
                {
                    "name": "web_search",
                    "type": "web_search_20250305",
                }
            ]

        return client.messages.create(**kwargs)
    except anthropic.AuthenticationError as e:
        raise InvalidAPIKeyError(f"Anthropic authentication failed: {e}") from e
    except anthropic.RateLimitError as e:
        raise APIError(f"Anthropic rate limit exceeded: {e}") from e
    except anthropic.APIConnectionError as e:
        raise APIError(f"Anthropic connection error: {e}") from e
    except anthropic.APIError as e:
        raise APIError(f"Anthropic API error: {e}") from e


def stream_google(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: int,
    temperature: float,
) -> Iterator["GenerateContentResponse"]:
    """Generate completions using Google's Gemini API."""
    check_dependency(genai, "Google GenAI package", "`pip install google‑genai`")
    validate_api_key(api_key, "Google")

    assert genai is not None
    assert genai_types is not None

    client = genai.Client(api_key=api_key)
    parsed_messages = parse_messages(prompt, {"user", "human", "assistant", "model"})
    contents: list[Any]
    if parsed_messages:
        contents = []
        for msg in parsed_messages:
            factory = (
                genai_types.UserContent
                if msg.role in {"user", "human"}
                else genai_types.ModelContent
            )
            contents.append(
                factory(parts=[genai_types.Part.from_text(text=msg.content)])
            )
    else:
        contents = [
            genai_types.UserContent(parts=[genai_types.Part.from_text(text=prompt)])
        ]

    generation_config_args: dict[str, Any] = {
        "max_output_tokens": min(DEFAULT_GOOGLE_MAX_TOKENS, max_tokens),
        "temperature": temperature,
    }

    try:
        config_params = {**generation_config_args}

        api_response = client.models.generate_content_stream(
            model=model,
            contents=contents,
            config=genai_types.GenerateContentConfig(**config_params),
        )
        return api_response
    except ValueError as e:
        if "api_key" in str(e).lower() or "authentication" in str(e).lower():
            raise InvalidAPIKeyError(f"Google API authentication failed: {e}") from e
        raise APIError(f"Google API parameter error: {e}") from e
    except ConnectionError as e:
        raise APIError(f"Google API connection error: {e}") from e
    except TimeoutError as e:
        raise APIError(f"Google API timeout: {e}") from e


def _handle_openai_stream(stream: OpenAIStream[Any]) -> str:
    """Handle OpenAI Responses API streaming."""
    out = ""
    current_tool_name: Optional[str] = None
    current_tool_args_buffer: str = ""
    in_reasoning = False
    
    import collections.abc as _abc

    if not isinstance(stream, _abc.Iterator):
        print(
            f"Warning: Received non-iterator OpenAI response: {stream}",
            flush=True
        )
        return ""

    for event in stream:
        event_type = getattr(event, "type", None)
        
        if event_type == "response.output_text.delta":
            delta_text = getattr(event, "delta", "")
            if delta_text:
                print(delta_text, end="", flush=True)
                out += delta_text
                
        elif event_type == "response.output_item.added":
            item = getattr(event, "item", None)
            if item:
                item_type = getattr(item, "type", None)
                if item_type == "reasoning":
                    print("\n[Thinking...]", flush=True)
                    in_reasoning = True
                elif item_type == "function_call":
                    current_tool_name = getattr(item, "name", None)
                    if current_tool_name and current_tool_name != "web_search_preview":
                        print(f"\n[Using tool: {current_tool_name}...]", flush=True)
                    current_tool_args_buffer = ""
                    
        elif event_type == "response.output_item.done":
            item = getattr(event, "item", None)
            if item:
                item_type = getattr(item, "type", None)
                if item_type == "reasoning" and in_reasoning:
                    print("\n[Thinking done.]\n", flush=True)
                    in_reasoning = False
                    
        elif event_type == "response.function_call_arguments.delta":
            delta_args = getattr(event, "delta", "")
            if delta_args:
                current_tool_args_buffer += delta_args
                
        elif event_type == "response.function_call_arguments.done":
            if current_tool_name == "web_search_preview" and current_tool_args_buffer:
                try:
                    import json
                    args_data = json.loads(current_tool_args_buffer)
                    search_query = args_data.get("query", "N/A")
                    print(f"\n[Searching for: \"{search_query}\"]", flush=True)
                except json.JSONDecodeError:
                    print(
                        f"[JSON decode error for tool args: {current_tool_args_buffer[:60]}...]",
                        flush=True
                    )
            elif current_tool_name:
                print(f"[Finished preparing {current_tool_name} call]", flush=True)
                
        elif event_type == "response.reasoning.delta":
            reasoning_text = getattr(event, "delta", "")
            if reasoning_text:
                print(reasoning_text, end="", flush=True)
                
        elif event_type == "response.web_search_call":
            print("[Got web search results]", flush=True)
            
        elif event_type == "response.error":
            error_details = getattr(event, "error", None)
            print(f"\nError in Responses API stream: {error_details}", flush=True)
            break
            
        elif event_type == "response.completed":
            pass
            
        elif event_type in ["response.created", "response.in_progress",
                            "response.content_part.added",
                            "response.output_text.done", "response.content_part.done"]:
            pass
    
    print()
    return out


def _handle_anthropic_stream(stream: Iterator["AnthropicMessageStreamEvent"]) -> str:
    out = ""
    current_block_type: Optional[str] = None
    current_tool_name: Optional[str] = None
    current_tool_input_buffer: str = ""

    for chunk in stream:
        chunk_type = getattr(chunk, "type", None)

        if chunk_type == "message_start":
            pass

        elif chunk_type == "content_block_start":
            content_block = getattr(chunk, "content_block", None)
            if content_block:
                current_block_type = getattr(content_block, "type", None)
                
                if current_block_type == "thinking":
                    print("\n[Thinking...]", flush=True)
                elif current_block_type == "server_tool_use":
                    tool_use_block = content_block
                    current_tool_name = getattr(tool_use_block, "name", None)
                    if current_tool_name == "web_search":
                        pass
                    elif current_tool_name:
                        print(f"\n[Using tool: {current_tool_name}...]", flush=True)
                    current_tool_input_buffer = ""
                elif current_block_type == "web_search_tool_result":
                    print("[Got web search results]", flush=True)

        elif chunk_type == "content_block_delta":
            delta = getattr(chunk, "delta", None)
            if delta:
                delta_type = getattr(delta, "type", None)

                if delta_type == "text_delta":
                    text = getattr(delta, "text", None)
                    if text:
                        print(text, end="", flush=True)
                        out += str(text)
                elif delta_type == "thinking_delta":
                    thinking_delta_obj = getattr(delta, "thinking", None)
                    if thinking_delta_obj:
                        text_content = str(thinking_delta_obj)
                        if text_content is not None:
                            print(str(text_content), end="", flush=True)
                elif delta_type == "input_json_delta":
                    if current_block_type == "server_tool_use":
                        partial_json = getattr(delta, "partial_json", None)
                        if partial_json:
                            current_tool_input_buffer += partial_json
        
        elif chunk_type == "content_block_stop":
            if current_block_type == "thinking":
                print("\n[Thinking done.]\n", flush=True)
            elif current_block_type == "server_tool_use":
                if current_tool_name == "web_search" and current_tool_input_buffer:
                    try:
                        import json
                        query_data = json.loads(current_tool_input_buffer)
                        search_query = query_data.get("query", "N/A")
                        print(f"\n[Searching for: \"{search_query}\"]", flush=True)
                    except json.JSONDecodeError:
                        print(
                            f"[JSON decode error for search query: {current_tool_input_buffer[:60]}...]",
                            flush=True
                        )
                elif current_tool_name:
                    print(f"[Finished using tool: {current_tool_name}]\n", flush=True)

                current_tool_name = None
                current_tool_input_buffer = ""
            elif current_block_type == "web_search_tool_result":
                pass
            
            current_block_type = None

        elif chunk_type == "message_delta":
            pass
        
        elif chunk_type == "message_stop":
            print()

    return out


def _handle_google_stream(stream: Iterator["GenerateContentResponse"]) -> str:
    """Handle Google streaming responses."""
    out = ""
    for item in stream:
        chunk: Any = item 
        text_val = getattr(chunk, "text", None)
        if text_val is not None:
            text = str(text_val)
            print(text, end="", flush=True)
            out += text
    return out


def print_and_collect(stream: Union[OpenAIStream[Any], Iterator[Any]], api_type: APIType) -> str:
    """Print streaming output and collect the complete response."""
    if api_type == "openai":
        return _handle_openai_stream(cast(OpenAIStream[Any], stream))

    if api_type == "anthropic":
        return _handle_anthropic_stream(cast(Iterator["AnthropicMessageStreamEvent"], stream))

    if api_type == "google":
        return _handle_google_stream(cast(Iterator["GenerateContentResponse"], stream))

    raise ValueError(f"Unsupported api_type {api_type}")


def write_jsonl(prompt: str, completion: str) -> None:
    """Write prompt and completion to JSONL file for logging purposes."""
    if jsonlines is None:
        return

    try:
        dst = Path.home() / JSONL_OUTPUT_FILE
        dst.touch(exist_ok=True)
        with jsonlines.open(dst, mode="a") as w:
            w.write({"prompt": prompt, "completion": completion})
    except (OSError, IOError) as e:
        print(f"Warning: Could not write to JSONL file: {e}", file=sys.stderr)
    except (ValueError, TypeError) as e:
        print(f"Warning: Could not serialize data to JSONL: {e}", file=sys.stderr)


def main() -> None:
    """Main entry point for the application."""
    try:
        args = parse_args()

        try:
            prompt = Path(args.prompt_file).read_text(encoding="utf-8")
        except OSError as err:
            raise APIError(f"Could not read prompt file: {err}") from err

        stream: Union[OpenAIStream[Any], Iterator[Any]]
        if args.api_type == "openai":
            stream = call_openai(
                prompt, args.api_key, args.model, args.max_tokens, args.temperature, args.web_search
            )
        elif args.api_type == "anthropic":
            stream = stream_anthropic(
                prompt,
                args.api_key,
                args.model,
                args.max_tokens,
                args.temperature,
                args.thinking_enabled,
                args.thinking_budget,
                args.interleaved_thinking,
                args.web_search,
            )
        else:
            stream = stream_google(
                prompt, args.api_key, args.model, args.max_tokens, args.temperature
            )

        completion = print_and_collect(stream, args.api_type)
        
        write_jsonl(prompt, completion)

    except KeyboardInterrupt:
        print("\nOperation cancelled by user.", file=sys.stderr)
        sys.exit(130)
    except (APIError, MissingDependencyError, InvalidAPIKeyError) as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {type(e).__name__}: {e}", file=sys.stderr)
        print("Please report this as a bug if it persists.", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
