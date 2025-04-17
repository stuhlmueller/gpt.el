#!/usr/bin/env python
# Copyright (C) 2022‑2025 Andreas Stuhlmueller – MIT License
"""
gpy.py – generate completions with OpenAI (Responses API + web‑search),
Anthropic, or Google Gemini.
"""

from __future__ import annotations

import sys
import argparse
import re
from pathlib import Path
from typing import (
    cast,
    TYPE_CHECKING,
    Any,
    Iterator,
    Literal,
    Optional,
    Union,
)

if TYPE_CHECKING:  # pragma: no cover
    from openai.types.responses import Response as OpenAIResponse
    from openai.types.responses.response_stream_event import (
        ResponseStreamEvent as OpenAIStreamEvent,
    )
    from anthropic.types import MessageStreamEvent as AnthropicMessageStreamEvent
    from google.genai.types import GenerateContentResponse

APIType = Literal["openai", "anthropic", "google"]

# A loose union that's strict enough for editors but doesn't require
# every provider's package to be installed at runtime.
CompletionStream = Union[
    "OpenAIResponse",
    Iterator["OpenAIStreamEvent"],
    Iterator["AnthropicMessageStreamEvent"],
    Iterator["GenerateContentResponse"],
]

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
    if openai is None:  # pragma: no cover
        sys.exit("openai‑python not installed – `pip install -U openai`")

    client = openai.OpenAI(api_key=api_key)

    # Convert "User: ... / Assistant: ..." blocks to message dicts
    messages: list[dict[str, str]] = [
        {"role": "system", "content": "You are a helpful assistant."}
    ]
    pattern = re.compile(
        r"^(User|Assistant):(.+?)(?=\n(?:User|Assistant):|\Z)", re.S | re.M
    )
    for m in pattern.finditer(prompt):
        messages.append({"role": m.group(1).lower(), "content": m.group(2).strip()})

    return client.responses.create(
        model=model,
        input=messages,
        tools=[{"type": "web_search_preview"}],  # built‑in web search
        max_output_tokens=max_tokens,
        stream=False,  # set True if you need chunked output
    )


def stream_anthropic(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: int,
    temperature: float,
) -> Iterator["AnthropicMessageStreamEvent"]:
    if anthropic is None:  # pragma: no cover
        sys.exit("anthropic‑python not installed – `pip install anthropic`")
    if api_key == "NOT SET":
        sys.exit("Anthropic API key not set.")

    client = anthropic.Anthropic(api_key=api_key)
    messages: list[dict[str, str]] = []
    pattern = re.compile(
        r"^(User|Assistant):(.+?)(?=\n(?:User|Assistant):|\Z)", re.S | re.M
    )
    current_user: Optional[str] = None
    for m in pattern.finditer(prompt):
        role = "user" if m.group(1).lower() == "user" else "assistant"
        content = m.group(2).strip()
        if role == "user":
            current_user = f"{current_user}\n\n{content}" if current_user else content
        else:
            if current_user:
                messages.append({"role": "user", "content": current_user})
                current_user = None
            messages.append({"role": "assistant", "content": content})
    if current_user:
        messages.append({"role": "user", "content": current_user})

    return client.messages.create(
        model=model,
        messages=messages,
        max_tokens=max_tokens,
        temperature=temperature,
        stream=True,
    )


def stream_google(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: int,
    temperature: float,
) -> Iterator["GenerateContentResponse"]:
    if genai is None:  # pragma: no cover
        sys.exit("google‑genai not installed – `pip install google‑genai`")

    client = genai.Client(api_key=api_key)

    pattern = re.compile(
        r"^(User|Human|Assistant|Model):(.+?)(?=\n(?:User|Human|Assistant|Model):|\Z)",
        re.S | re.M,
    )
    contents: list[Any]
    if pattern.search(prompt):
        # conversation
        contents = []
        for m in pattern.finditer(prompt):
            role = m.group(1).lower()
            txt = m.group(2).strip()
            factory = (
                genai_types.UserContent
                if role in {"user", "human"}
                else genai_types.ModelContent
            )
            contents.append(factory(parts=[genai_types.Part.from_text(text=txt)]))
    else:
        # single‑shot prompt
        contents = [
            genai_types.UserContent(
                parts=[genai_types.Part.from_text(text=prompt)]
            )
        ]

    return client.models.generate_content_stream(
        model=model,
        contents=contents,
        config=genai_types.GenerateContentConfig(
            max_output_tokens=max(32000, max_tokens),
            temperature=temperature,
        ),
    )


def print_and_collect(stream: CompletionStream, api_type: APIType) -> str:
    out = ""

    if api_type == "openai":
        # non‑stream Responses
        if hasattr(stream, "output_text"):
            text = str(getattr(stream, "output_text", ""))
            print(text, end="", flush=True)
            return text
        # stream Responses
        for event in cast(Iterator[Any], stream):
            # Some event types in the OpenAI SDK do **not** expose the
            # ``event`` or ``data`` attributes (e.g. audio‑related events).
            # Static analyzers therefore complain when accessing them on the
            # union type. We therefore get the attributes dynamically.
            if getattr(event, "event", None) == "content_block_delta":
                data = getattr(event, "data", None)
                delta = getattr(data, "delta", None) if data else None
                content = getattr(delta, "content", None) if delta else None
                if content:
                    print(content, end="", flush=True)
                    out += str(content)
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
    if jsonlines is None:
        return
    dst = Path.home() / ".emacs_prompts_completions.jsonl"
    dst.touch(exist_ok=True)
    with jsonlines.open(dst, mode="a") as w:
        w.write({"prompt": prompt, "completion": completion})


def main() -> None:
    args = parse_args()

    try:
        prompt = Path(args.prompt_file).read_text(encoding="utf-8")
    except OSError as err:  # pragma: no cover
        sys.exit(f"Could not read prompt file: {err}")

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

    completion = print_and_collect(stream, args.api_type)  # type: ignore[arg-type]
    write_jsonl(prompt, completion)


if __name__ == "__main__":
    main()
