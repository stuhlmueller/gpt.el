#!/usr/bin/env python

# Copyright (C) 2022 Andreas Stuhlmueller
# License: MIT
# SPDX-License-Identifier: MIT

import sys
import os
import argparse

from pathlib import Path

try:
    import openai
except ImportError:
    print("gpt.el requires the OpenAI Python package.")
    print("Please install, for example by running `pip install openai'.")
    sys.exit(1)

try:
    import jsonlines
except ImportError:
    jsonlines = None


def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument("api_key", help="The API key to use for the OpenAI API.")
    parser.add_argument("engine", help="The engine to use for the OpenAI API.")
    parser.add_argument("max_tokens", help="Max tokens value to be used with the OpenAI API..")
    parser.add_argument("temperature", help="Temperature value to be used with the OpenAI API..")
    parser.add_argument("prompt_file", help="The file that contains the prompt.")
    return parser.parse_args()


def read_input_text() -> str:
    """Read input text from stdin."""
    lines = []
    for line in sys.stdin.readlines():
        lines.append(line)
    return "".join(lines)


def stream_completions(prompt: str, api_key: str, engine: str, max_tokens: str, temperature: str) -> openai.Completion:
    """Stream completions from the openai API."""
    if api_key == "NOT SET":
        print("Error: API key not set.")
        print(
            'Add (setq gpt-openai-key "sk-Aes.....AV8qzL") to your Emacs init.el file.'
        )
        sys.exit(1)
    openai.api_key = api_key
    try:
        return openai.Completion.create(
            engine=engine,
            prompt=prompt,
            max_tokens=int(max_tokens),
            temperature=float(temperature),
            stream=True,
        )
    except openai.error.APIError as e:
        print(f"Error: {e}")
        sys.exit(1)


def print_and_collect_completions(stream: openai.Completion) -> str:
    """Print and collect completions from the stream."""
    completion_text = ""
    for i, completion in enumerate(stream):
        this_text = completion.choices[0].text
        if i == 0:
            this_text = this_text.lstrip("\n")
        print(this_text, end="", flush=True)
        completion_text += completion.choices[0].text
    return completion_text


def write_to_jsonl(prompt: str, completion: str, path: Path) -> None:
    """Write the prompt and completion to a jsonl file."""
    if jsonlines is None:
        return
    if not os.path.exists(path):
        with open(path, "w") as f:
            pass  # Create the file
    try:
        with jsonlines.open(path, mode="a") as writer:
            writer.write({"prompt": prompt, "completion": completion})
    except IOError as e:
        print(f"Error: {e}")
        sys.exit(1)


def main() -> None:
    args = parse_args()
    with open(args.prompt_file, "r") as f:
        prompt = f.read()
    stream = stream_completions(prompt, args.api_key, args.engine, args.max_tokens, args.temperature)
    completion_text = print_and_collect_completions(stream)
    file_name = Path.home() / ".emacs_prompts_completions.jsonl"
    write_to_jsonl(prompt, completion_text, file_name)


if __name__ == "__main__":
    main()
