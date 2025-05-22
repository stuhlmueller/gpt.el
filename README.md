# GPT CLI Tool

A unified command-line interface for generating text completions using OpenAI, Anthropic, or Google Gemini APIs.

## Features

- ✅ Support for multiple AI providers (OpenAI, Anthropic, Google)
- ✅ Streaming responses with real-time output
- ✅ Conversation format parsing
- ✅ Automatic logging to JSONL format
- ✅ Web search integration (OpenAI)
- ✅ Robust error handling

## Installation

```bash
pip install -r requirements.txt
```

## Usage

```bash
python gpt.py <api_key> <model> <max_tokens> <temperature> <api_type> <prompt_file>
```

### Examples

```bash
# OpenAI GPT-4
python gpt.py "your-openai-key" "gpt-4" 1000 0.7 openai prompt.txt

# Anthropic Claude
python gpt.py "your-anthropic-key" "claude-3-sonnet-20240229" 1000 0.7 anthropic prompt.txt

# Google Gemini
python gpt.py "your-google-key" "gemini-1.5-pro" 1000 0.7 google prompt.txt
```

## Prompt Format

Supports conversation format:

```
User: What is the capital of France?
```
