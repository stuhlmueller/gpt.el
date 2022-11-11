#!/usr/bin/env python

import sys
import os
import argparse

# import jsonlines
import openai


def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument("command", help="The command to pass to the model.")
    parser.add_argument("api_key", help="The API key to use for the OpenAI API.")
    parser.add_argument("engine", help="The engine to use for the OpenAI API.")
    return parser.parse_args()


def read_input_text() -> str:
    """Read input text from stdin."""
    lines = []
    for line in sys.stdin.readlines():
        lines.append(line)
    return "".join(lines)


def generate_prompt(input_text: str, command: str) -> str:
    """Generate a prompt from input text and command."""
    if input_text.strip():
        return f'''
"""
{input_text}
"""

{command}<|endofprompt|>'''
    return command


def stream_completions(prompt: str, api_key: str, engine: str) -> openai.Completion:
    """Stream completions from the openai API."""
    if api_key == "NOT SET":
        print("Error: API key not set.")
        print(f'Add (setq gpt-openai-key "sk-Aes.....AV8qzL") to your .emacs file.')
        sys.exit(1)
    openai.api_key = api_key
    try:
        return openai.Completion.create(
            engine=engine,
            prompt=prompt,
            max_tokens=2000,
            temperature=0,
            stream=True,
        )
    except openai.error.APIError as e:
        print(f"Error: {e}")
        sys.exit(1)


def print_and_collect_completions(stream: openai.Completion) -> str:
    """Print and collect completions from the stream."""
    completion_text = ""
    for completion in stream:
        print(completion.choices[0].text, end="", flush=True)
        completion_text += completion.choices[0].text
    return completion_text


# def write_to_jsonl(prompt: str, completion: str, file_name: str) -> None:
#     """Write the prompt and completion to a jsonl file."""
#     try:
#         with jsonlines.open(file_name, mode="a") as writer:
#             writer.write({"prompt": prompt, "completion": completion})
#     except IOError as e:
#         print(f"Error: {e}")
#         sys.exit(1)


def main() -> None:
    # Parse command line arguments
    args = parse_args()

    # Read input text from stdin
    input_text = read_input_text()

    # Generate the prompt from input text and command
    prompt = generate_prompt(input_text, args.command)

    # Stream the completions from the openai API
    stream = stream_completions(prompt, args.api_key, args.engine)

    # Print and collect the completions
    completion_text = print_and_collect_completions(stream)

    # Write the prompt and completion to a jsonl file
    # file_name = "/Users/stuhlmueller/emacs_prompts_completions.jsonl"
    # write_to_jsonl(prompt, completion_text, file_name)


if __name__ == "__main__":
    main()
