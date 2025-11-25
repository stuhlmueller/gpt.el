#!/usr/bin/env python
# Copyright (C) 2022‑2025 Andreas Stuhlmueller – MIT License
"""
gpt.py – Generate completions with OpenAI (Responses API + web‑search),
Anthropic (with extended thinking), or Google Gemini.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterator, Literal, Optional, Union, cast

from providers.anthropic_provider import handle_anthropic_stream, stream_anthropic

# Import provider modules
from providers.common import (
    DEFAULT_THINKING_BUDGET,
    APIError,
    InvalidAPIKeyError,
    MissingDependencyError,
)
from providers.google_provider import handle_google_stream, stream_google
from providers.openai_provider import call_openai, handle_openai_stream

# Third-party imports (optional)
jsonlines: Optional[Any] = None

try:
    import jsonlines
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
JSONL_OUTPUT_FILE = ".emacs_prompts_completions.jsonl"

APIType = Literal["openai", "anthropic", "google"]

CompletionStream = Union[
    "OpenAIStream[Any]",
    Iterator["AnthropicMessageStreamEvent"],
    Iterator["GenerateContentResponse"],
    Any,
]


def parse_args() -> argparse.Namespace:
    """Parse command-line arguments for the completion generator."""
    p = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description="Generate completions with OpenAI, Anthropic, or Google APIs.",
    )
    p.add_argument("model", help="Model name to use for completion")
    p.add_argument("max_tokens", type=int, help="Maximum tokens to generate")
    p.add_argument("temperature", type=float, help="Temperature for sampling (0.0-2.0)")
    p.add_argument(
        "api_type",
        choices=("openai", "anthropic", "google"),
        help="API provider to use",
    )
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


def print_and_collect(stream: Union["OpenAIStream[Any]", Iterator[Any]], api_type: APIType) -> str:
    """Print streaming output and collect the complete response."""
    out = ""

    # Get the appropriate handler based on API type
    if api_type == "openai":
        string_stream = handle_openai_stream(cast("OpenAIStream[Any]", stream))
    elif api_type == "anthropic":
        string_stream = handle_anthropic_stream(cast(Iterator["AnthropicMessageStreamEvent"], stream))
    elif api_type == "google":
        string_stream = handle_google_stream(cast(Iterator["GenerateContentResponse"], stream))
    else:
        raise ValueError(f"Unsupported api_type {api_type}")

    # Print and collect the strings from the generator
    for text in string_stream:
        print(text, end="", flush=True)
        out += text

    # Print final newline if output doesn't end with one
    if out and not out.endswith("\n"):
        print()

    return out


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

        # Read API key from stdin (more secure than command line or env vars)
        api_key = sys.stdin.readline().rstrip("\n")
        if not api_key:
            raise APIError("No API key provided via stdin")

        try:
            prompt = Path(args.prompt_file).read_text(encoding="utf-8")
        except OSError as err:
            raise APIError(f"Could not read prompt file: {err}") from err

        stream: Union["OpenAIStream[Any]", Iterator[Any]]
        if args.api_type == "openai":
            stream = call_openai(
                prompt,
                api_key,
                args.model,
                args.max_tokens,
                args.temperature,
                args.web_search,
            )
        elif args.api_type == "anthropic":
            stream = stream_anthropic(
                prompt,
                api_key,
                args.model,
                args.max_tokens,
                args.temperature,
                args.thinking_enabled,
                args.thinking_budget,
                args.interleaved_thinking,
                args.web_search,
            )
        else:
            stream = stream_google(prompt, api_key, args.model, args.max_tokens, args.temperature)

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
