#!/usr/bin/env python
# Copyright (C) 2022‑2025 Andreas Stuhlmueller – MIT License
"""
gpy.py – generate completions with OpenAI (Responses API + web‑search),
Anthropic, or Google Gemini.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterator, Literal, Optional, Union, cast, TYPE_CHECKING

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
    from anthropic.types import MessageStreamEvent as AnthropicMessageStreamEvent
    from google.genai.types import GenerateContentResponse
    from openai.types.responses import Response as OpenAIResponse
    from openai.types.responses.response_stream_event import (
        ResponseStreamEvent as OpenAIStreamEvent,
    )

# Constants
DEFAULT_SYSTEM_MESSAGE = "You are a helpful assistant."
JSONL_OUTPUT_FILE = ".emacs_prompts_completions.jsonl"
DEFAULT_GOOGLE_MAX_TOKENS = 32000

APIType = Literal["openai", "anthropic", "google"]

# A loose union that's strict enough for editors but doesn't require
# every provider's package to be installed at runtime.
CompletionStream = Union[
    "OpenAIResponse",
    Iterator["OpenAIStreamEvent"],
    Iterator["AnthropicMessageStreamEvent"],
    Iterator["GenerateContentResponse"],
]


# Data structures
@dataclass
class Message:
    """Represents a chat message with role and content."""
    role: str
    content: str


# Custom exceptions
class APIError(Exception):
    """Base exception for API-related errors."""
    pass


class MissingDependencyError(APIError):
    """Raised when a required package is not installed."""
    pass


class InvalidAPIKeyError(APIError):
    """Raised when an API key is invalid or not set."""
    pass


# Utility functions
def parse_messages(prompt: str, supported_roles: set[str] | None = None) -> list[Message]:
    """Parse a prompt string into a list of Message objects.
    
    Args:
        prompt: The prompt string containing role-based messages
        supported_roles: Set of supported roles (case-insensitive). If None, defaults to common roles.
    
    Returns:
        List of Message objects parsed from the prompt
    """
    if supported_roles is None:
        supported_roles = {"user", "assistant", "human", "model"}
    
    # Create pattern for supported roles
    role_pattern = "|".join(supported_roles)
    pattern = re.compile(
        rf"^({role_pattern}):(.+?)(?=\n(?:{role_pattern}):|\Z)", 
        re.S | re.M | re.I
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
    p = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description="Generate completions with OpenAI, Anthropic, or Google APIs.",
    )
    p.add_argument("api_key")
    p.add_argument("model")
    p.add_argument("max_tokens", type=int)
    p.add_argument("temperature", type=float)
    p.add_argument("api_type", choices=("openai", "anthropic", "google"))
    p.add_argument("prompt_file")
    return p.parse_args()


def call_openai(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: int,
    temperature: float,
) -> "OpenAIResponse":
    """Generate completions using OpenAI's API."""
    check_dependency(openai, "OpenAI Python package", "`pip install -U openai`")
    validate_api_key(api_key, "OpenAI")

    # At this point, openai is guaranteed to be available due to check_dependency
    assert openai is not None
    
    client = openai.OpenAI(api_key=api_key)
    
    # Parse messages and convert to OpenAI format
    parsed_messages = parse_messages(prompt, {"user", "assistant"})
    messages: list[dict[str, str]] = [
        {"role": "system", "content": DEFAULT_SYSTEM_MESSAGE}
    ]
    
    for msg in parsed_messages:
        messages.append({"role": msg.role, "content": msg.content})

    try:
        return client.responses.create(
            model=model,
            input=messages,
            # tools=[{"type": "web_search_preview"}],  # built‑in web search
            max_output_tokens=max_tokens,
            stream=True,
        )
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
) -> Iterator["AnthropicMessageStreamEvent"]:
    """Generate completions using Anthropic's API."""
    check_dependency(anthropic, "Anthropic Python package", "`pip install anthropic`")
    validate_api_key(api_key, "Anthropic")
    
    # At this point, anthropic is guaranteed to be available
    assert anthropic is not None

    client = anthropic.Anthropic(api_key=api_key)
    
    # Parse messages and handle Anthropic's message format requirements
    parsed_messages = parse_messages(prompt, {"user", "assistant"})
    messages: list[dict[str, str]] = []
    
    # Anthropic requires alternating user/assistant messages and consolidates consecutive user messages
    current_user_content: Optional[str] = None
    
    for msg in parsed_messages:
        if msg.role == "user":
            if current_user_content:
                current_user_content += f"\n\n{msg.content}"
            else:
                current_user_content = msg.content
        else:  # assistant
            if current_user_content:
                messages.append({"role": "user", "content": current_user_content})
                current_user_content = None
            messages.append({"role": "assistant", "content": msg.content})
    
    # Add any remaining user content
    if current_user_content:
        messages.append({"role": "user", "content": current_user_content})

    try:
        return client.messages.create(
            model=model,
            messages=messages,
            max_tokens=max_tokens,
            temperature=temperature,
            stream=True,
        )
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
    
    # At this point, genai is guaranteed to be available
    assert genai is not None

    client = genai.Client(api_key=api_key)

    # Parse messages and convert to Google format
    parsed_messages = parse_messages(prompt, {"user", "human", "assistant", "model"})
    
    contents: list[Any]
    if parsed_messages:
        # Conversation mode
        contents = []
        for msg in parsed_messages:
            factory = (
                genai_types.UserContent
                if msg.role in {"user", "human"}
                else genai_types.ModelContent
            )
            contents.append(factory(parts=[genai_types.Part.from_text(text=msg.content)]))
    else:
        # Single-shot prompt mode
        contents = [
            genai_types.UserContent(
                parts=[genai_types.Part.from_text(text=prompt)]
            )
        ]

    try:
        return client.models.generate_content_stream(
            model=model,
            contents=contents,
            config=genai_types.GenerateContentConfig(
                max_output_tokens=max(DEFAULT_GOOGLE_MAX_TOKENS, max_tokens),
                temperature=temperature,
            ),
        )
    except ValueError as e:
        # Google GenAI often raises ValueError for invalid parameters/auth
        if "api_key" in str(e).lower() or "authentication" in str(e).lower():
            raise InvalidAPIKeyError(f"Google API authentication failed: {e}") from e
        raise APIError(f"Google API parameter error: {e}") from e
    except ConnectionError as e:
        raise APIError(f"Google API connection error: {e}") from e
    except TimeoutError as e:
        raise APIError(f"Google API timeout: {e}") from e


def print_and_collect(stream: CompletionStream, api_type: APIType) -> str:
    out = ""

    if api_type == "openai":
        # Handle both streaming and non-streaming Responses API outputs
        if not isinstance(stream, Iterator):  # Non-streaming case
            if hasattr(stream, "output_text"):
                text = str(getattr(stream, "output_text", ""))
                print(text, end="", flush=True)
                return text
            else:
                # Handle potential non-streaming responses without output_text if needed
                print(f"Warning: Received non-iterator OpenAI response with no output_text: {stream}", file=sys.stderr)
                return ""

        # Streaming case
        current_web_search_query = None
        for event in cast(Iterator[Any], stream):
            event_type = getattr(event, "type", None)

            # Try to capture the query when the web search item is added
            if event_type == "response.output_item.added":
                item = getattr(event, "item", None)
                if item and getattr(item, "type", None) == "web_search_call":
                    # 1) direct attribute
                    q = getattr(item, "query", None)

                    # 2) arguments dict (preferred)
                    if q is None:
                        args = getattr(item, "arguments", None)
                        if isinstance(args, dict):
                            q = args.get("query")

                    # 3) .model_dump() fallback
                    if q is None and hasattr(item, "model_dump"):
                        dump = item.model_dump()
                        q = (
                            dump.get("query")
                            or dump.get("arguments", {}).get("query")
                            or dump.get("parameters", {}).get("query")
                        )

                    current_web_search_query = q

            elif event_type == "response.web_search_call.searching":
                search_msg = "[Searching the web"
                if current_web_search_query:
                    # Use repr() to see quotes around the query if it's found
                    search_msg += f": {repr(current_web_search_query)}"
                search_msg += "...]"
                # Print progress to stderr on its own line
                print(search_msg, file=sys.stderr)
                current_web_search_query = None  # Reset after displaying

            elif event_type == "response.output_text.delta":
                delta_text = getattr(event, "delta", "")
                if delta_text:
                    # Print final response text to stdout
                    print(delta_text, end="", flush=True)
                    out += delta_text
            # Add elif conditions here for other event types you want to track
            # e.g., elif event_type == "response.reasoning_step.start": print("[Thinking...]")

        # Ensure a newline after streaming is complete
        print()
        return out

    if api_type == "anthropic":
        for chunk in cast(Iterator[Any], stream):
            if getattr(chunk, "type", None) == "content_block_delta":
                delta = getattr(chunk, "delta", None)
                text = getattr(delta, "text", None) if delta else None
                if text:
                    print(text, end="", flush=True)
                    out += str(text)
        return out

    if api_type == "google":
        for chunk in cast(Iterator[Any], stream):
            text = getattr(chunk, "text", None)
            if text:
                print(text, end="", flush=True)
                out += str(text)
        return out

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
        # Handle file system errors (permissions, disk full, etc.)
        print(f"Warning: Could not write to JSONL file: {e}", file=sys.stderr)
    except (ValueError, TypeError) as e:
        # Handle JSON serialization errors
        print(f"Warning: Could not serialize data to JSONL: {e}", file=sys.stderr)


def main() -> None:
    """Main entry point for the application."""
    try:
        args = parse_args()

        # Read prompt file
        try:
            prompt = Path(args.prompt_file).read_text(encoding="utf-8")
        except OSError as err:
            raise APIError(f"Could not read prompt file: {err}") from err

        # Generate completion based on API type
        if args.api_type == "openai":
            stream = call_openai(
                prompt, args.api_key, args.model, args.max_tokens, args.temperature
            )
        elif args.api_type == "anthropic":
            stream = stream_anthropic(
                prompt, args.api_key, args.model, args.max_tokens, args.temperature
            )
        else:  # google
            stream = stream_google(
                prompt, args.api_key, args.model, args.max_tokens, args.temperature
            )

        # Process and display the completion
        completion = print_and_collect(stream, args.api_type)  # type: ignore[arg-type]
        
        # Log the interaction
        write_jsonl(prompt, completion)
        
    except KeyboardInterrupt:
        print("\nOperation cancelled by user.", file=sys.stderr)
        sys.exit(130)  # Standard exit code for SIGINT
    except (APIError, MissingDependencyError, InvalidAPIKeyError) as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        # This should only catch truly unexpected errors
        print(f"Unexpected error: {type(e).__name__}: {e}", file=sys.stderr)
        print("Please report this as a bug if it persists.", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
