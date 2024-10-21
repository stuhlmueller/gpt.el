#!/usr/bin/env python

# Copyright (C) 2022 Andreas Stuhlmueller
# License: MIT
# SPDX-License-Identifier: MIT

import sys
import os
import argparse
import re
from pathlib import Path
from typing import Union

APIType = Union["openai", "anthropic"]

openai = None
anthropic = None

try:
    import openai
except ImportError:
    pass

try:
    import anthropic
except ImportError:
    pass

try:
    import jsonlines
except ImportError:
    jsonlines = None

def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", help="Which subcommand to run", required=True)
    chat_parser = subparsers.add_parser("chat")
    subparsers.add_parser("complete")
    chat_parser.add_argument("prompt_file", help="The file that contains the prompt.")
    parser.add_argument("api_key", help="The API key to use for the selected API.")
    parser.add_argument("model", help="The model to use (e.g., 'gpt-4', 'claude-3-sonnet-20240229').")
    parser.add_argument("max_tokens", help="Max tokens value to be used with the API.")
    parser.add_argument("temperature", help="Temperature value to be used with the API.")
    parser.add_argument("api_type", type=str, choices=("openai", "anthropic"), help="The type of API to use: 'openai' or 'anthropic'.")
    return parser.parse_args()

def read_input_text() -> str:
    """Read input text from stdin."""
    return sys.stdin.read()

def stream_openai_chat_completions(
    prompt: str, api_key: str, model: str, max_tokens: str, temperature: str
) -> openai.Stream:
    """Stream chat completions from the OpenAI API."""
    if openai is None:
        print("Error: OpenAI Python package is not installed.")
        print("Please install by running `pip install openai'.")
        sys.exit(1)

    if api_key == "NOT SET":
        print("Error: OpenAI API key not set.")
        print('Add (setq gpt-openai-key "sk-Aes.....AV8qzL") to your Emacs init.el file.')
        sys.exit(1)

    client = openai.OpenAI(api_key=api_key)

    messages = [{"role": "system", "content": "You are a helpful assistant."}]
    pattern = re.compile(
        r"^(User|Assistant):(.+?)(?=\n(?:User|Assistant):|\Z)", re.MULTILINE | re.DOTALL
    )
    matches = pattern.finditer(prompt)
    for match in matches:
        role = match.group(1).lower()
        content = match.group(2).strip()
        messages.append({"role": role, "content": content})

    try:
        return client.chat.completions.create(
            model=model,
            messages=messages,
            max_tokens=int(max_tokens),
            temperature=float(temperature),
            stream=True,
        )
    except openai.APIError as error:
        print(f"Error: {error}")
        sys.exit(1)

def stream_anthropic_chat_completions(
    prompt: str, api_key: str, model: str, max_tokens: str, temperature: str
) -> anthropic.Anthropic:
    """Stream chat completions from the Anthropic API."""
    if anthropic is None:
        print("Error: Anthropic Python package is not installed.")
        print("Please install by running `pip install anthropic'.")
        sys.exit(1)

    if api_key == "NOT SET":
        print("Error: Anthropic API key not set.")
        print('Add (setq gpt-anthropic-key "sk-ant-api03-...") to your Emacs init.el file.')
        sys.exit(1)

    client = anthropic.Anthropic(api_key=api_key)

    messages = []
    pattern = re.compile(
        r"^(User|Assistant):(.+?)(?=\n(?:User|Assistant):|\Z)", re.MULTILINE | re.DOTALL
    )
    matches = pattern.finditer(prompt)

    # Anthropic requires alternating user and assistant messages,
    # so we group user messages together
    current_user_message = None

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

def print_and_collect_completions(stream, api_type: APIType) -> str:
    """Print and collect completions from the stream."""
    completion_text = ""
    if api_type == "openai":
        for chunk in stream:
            if chunk.choices[0].delta.content is not None:
                text = chunk.choices[0].delta.content
                print(text, end="", flush=True)
                completion_text += text
    elif api_type == "anthropic":
        for chunk in stream:
            if chunk.type == "content_block_delta":
                text = chunk.delta.text
                print(text, end="", flush=True)
                completion_text += text
    else:
        raise ValueError(f"Unsupported API type '{api_type}'")

    return completion_text

def write_to_jsonl(prompt: str, completion: str, path: Path) -> None:
    """Write the prompt and completion to a jsonl file."""
    if jsonlines is None:
        return
    if not os.path.exists(path):
        with open(path, "w", encoding="utf-8"):
            pass  # Create the file
    try:
        with jsonlines.open(path, mode="a") as writer:
            writer.write({"prompt": prompt, "completion": completion})
    except IOError as error:
        print(f"Error: {error}")
        sys.exit(1)

def stream_chat(
    prompt_file: str,
    api_key: str,
    api_type: str,
    model: str,
    max_tokens: str,
    temperature: str,
) -> None:

    with open(prompt_file, "r") as fdes:
        prompt = fdes.read()

    if api_type == "openai":
        stream = stream_openai_chat_completions(
            prompt, api_key, model, max_tokens, temperature
        )
    elif api_type == "anthropic":
        stream = stream_anthropic_chat_completions(
            prompt, api_key, model, max_tokens, temperature
        )
    else:
        print(f"Error: Unsupported API type '{api_type}'")
        sys.exit(1)
    completion_text = print_and_collect_completions(stream, api_type)
    file_name = Path.home() / ".emacs_prompts_completions.jsonl"
    write_to_jsonl(prompt, completion_text, file_name)


def complete_prompt(
    api_key: str, api_type: str, model: str, max_tokens: str, temperature: str
) -> None:
    prompt = read_input_text()
    if api_type == "openai":
        client = openai.OpenAI(api_key=api_key)
        req_fn = client.chat.completions.create
        err = openai.APIError
    else:
        client = anthropic.Anthropic(api_key=api_key)
        req_fn = client.messages.create
        err = anthropic.APIError
    messages = []
    messages.append(
        {
            "role": "system",
            "content": "Complete the following without including anything else, e.g., no comments, no triple backticks.",
        }
    )
    messages.append({"role": "user", "content": prompt})

    try:
        stream = req_fn(
            model=model,
            messages=messages,
            max_tokens=int(max_tokens),
            temperature=float(temperature),
            stream=True
        )
        print_and_collect_completions(stream, api_type)
    except err as exception:
        print(f"Error: {exception}")


def main() -> None:
    """
    Main function to read a prompt from a file, generate completions
    using the specified API, and save the completions to a JSONL file.
    """
    args = parse_args()
    if args.command == "chat":
        stream_chat(
            args.prompt_file,
            args.api_key,
            args.api_type,
            args.model,
            args.max_tokens,
            args.temperature,
        )
    elif args.command == "complete":
        complete_prompt(
            args.api_key, args.api_type, args.model, args.max_tokens, args.temperature
        )
    else:
        print(f"Error: Unsupported command '{args.command}'")


if __name__ == "__main__":
    main()
