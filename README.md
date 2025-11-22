# gpt.el

<p align="center">
  <img src="gpt.gif" alt="gpt.el demo" width="600"/>
</p>

gpt.el is an Emacs package that lets you interact with instruction-following language models like GPT-4.5, o3, Claude 4 Sonnet, Claude 4 Opus, and Gemini 2.5 Pro from your editor. You can type a natural language command (with history and completion support) and optionally use the current region or buffer contents as input for the model. The package displays the output of the model in a temporary or named buffer, and updates it as the model generates more text. You can issue follow-up commands that provide the interaction history in that buffer as context. You can also browse, save, and clear the command history for later reference.

## Features

- **Multi-provider support**: OpenAI, Anthropic, and Google Gemini APIs
- **Seamless Emacs integration**: Use current buffer/region as context
- **Streaming responses**: Real-time output as the model generates text
- **Interactive conversations**: Follow-up commands with conversation history
- **Command history**: Browse, save, and clear your command history
- **Flexible context modes**: All buffers, current buffer, or no context
- **Extended thinking mode**: Enhanced reasoning for Anthropic models with streaming thought process
- **Web search**: Real-time web search capability for Anthropic models

## Installation

### Prerequisites

You need to have at least one of the `openai`, `anthropic`, or `google-genai` Python packages as well as `jsonlines`. You'll also need valid API keys for OpenAI, Anthropic, or Google.

#### Python Dependencies

**If installing from source** and you have `uv` (recommended):

```bash
uv sync
```

**If you don't have `uv`, install it first:**

```bash
brew install uv  # On macOS
# or
pip install uv
```

**Alternative: Using pip with virtual environment:**

```bash
python3 -m venv .venv
source .venv/bin/activate  # On macOS/Linux
# or .venv\Scripts\activate  # On Windows
pip install -r requirements.txt
```

**Note:** On modern macOS systems, you cannot install packages globally with pip due to [PEP 668](https://peps.python.org/pep-0668/). The package automatically detects your virtual environment if you're installing from source.

#### API Keys

You can get API keys from:

- OpenAI: https://platform.openai.com/
- Anthropic: https://console.anthropic.com/
- Google Gemini: https://aistudio.google.com/app/apikey

### From MELPA (Recommended)

MELPA is a popular third-party package repository for Emacs. To install gpt.el from MELPA, first add MELPA as a source in your Emacs init file:

```elisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
```

Then, use the built-in package manager to install gpt.el:

```
M-x package-install RET gpt RET
```

### From Source

To install gpt.el from source, clone this repository and add the following to your Emacs init file:

```elisp
(add-to-list 'load-path "/path/to/gpt.el")
(require 'gpt)
```

Alternatively, you can use `use-package`:

```elisp
(use-package gpt
  :load-path "/path/to/gpt.el")
```

## Configuration

You need to set at least one of the variables `gpt-openai-key`, `gpt-anthropic-key`, or `gpt-google-key` to your respective API keys:

```elisp
(setq gpt-openai-key "sk-...")
(setq gpt-anthropic-key "sk-ant-api03-...")
(setq gpt-google-key "AIzaSy...")
```

Optionally, customize the model parameters:

```elisp
(setq gpt-model "claude-3-7-sonnet-latest")  ; Default model
(setq gpt-max-tokens "64000")  ; Automatically set based on model
(setq gpt-temperature "0")
```

### Python Path Configuration

The package automatically detects your Python executable in this order:

1. Virtual environment (`.venv/bin/python`) if installing from source
2. System `python3` command
3. System `python` command

If you need to specify a custom Python path:

```elisp
(setq gpt-python-path "/path/to/your/python")
```

### Available Models

gpt.el supports the latest models from all providers:

**OpenAI:**

- `gpt-4.1` - GPT-4.1
- `gpt-4.5-preview` - GPT-4.5 Preview
- `o3` - o3 (latest reasoning model)
- `o4-mini` - o4-mini

**Anthropic:**

- `claude-3-7-sonnet-latest` - Claude 3.7 Sonnet (default)
- `claude-sonnet-4-0` - Claude 4 Sonnet
- `claude-opus-4-0` - Claude 4 Opus

**Google:**

- `gemini-2.5-pro-preview-03-25` - Gemini 2.5 Pro Preview

You can switch models interactively with `M-x gpt-switch-model` or `C-c C-m` in gpt-mode buffers.

To switch API providers:

```elisp
;; For Anthropic (default)
(setq gpt-api-type 'anthropic)

;; For OpenAI
(setq gpt-api-type 'openai)

;; For Google Gemini
(setq gpt-api-type 'google)
```

## Usage

### Main Commands

- `gpt-chat` - Interactive prompt to choose context mode and enter command
- `gpt-chat-multi-models` - Run the same command against multiple models (parallel buffers). Uses defaults from `gpt-multi-models-default` (GPT-5 and Claude 4.1 Opus). Use `C-u` to pick models interactively.
- `gpt-chat-all-buffers` - Use all visible buffers as context
- `gpt-chat-current-buffer` - Use only the current buffer as context
- `gpt-edit-current-buffer` - Rewrite the current buffer with GPT, review the diff, and accept or reject the changes
- After generating a diff, `gpt-edit-current-buffer` also lets you supply additional feedback to iterate until the result looks right.
- `gpt-chat-no-context` - Use no buffer context

### Key Bindings

You can bind these functions to keys of your choice:

```elisp
(global-set-key (kbd "M-C-g") 'gpt-chat)
(global-set-key (kbd "M-C-b") 'gpt-chat-all-buffers)
(global-set-key (kbd "M-C-c") 'gpt-chat-current-buffer)
(global-set-key (kbd "M-C-e") 'gpt-edit-current-buffer)
```

### GPT Mode Key Bindings

When in a GPT output buffer (`gpt-mode`), these keys are available:

- `C-c C-c` - Follow-up command with conversation history
- `C-c C-p` - Toggle visibility of User:/Assistant: prefixes
- `C-c C-b` - Copy code block at point to clipboard
- `C-c C-m` - Switch between models interactively
- `C-c C-t` - Generate a title for the current buffer
- `C-c C-k` - Kill the running GPT process in this buffer
- `C-c C-q` - Close current GPT buffer
- `C-c C-x` - Close all GPT buffers
- `C-c C-r` - Regenerate the last assistant response

Thinking mode commands:

- `C-c C-j t` - Toggle extended thinking mode
- `C-c C-j i` - Toggle interleaved thinking mode
- `C-c C-j w` - Toggle web search
- `C-c C-j s` - Show current thinking mode status
- `C-c C-j m` - Run the same command across multiple models (uses defaults; `C-u` to pick)

### Context Modes

- **all-buffers**: Uses all visible buffers as context, with cursor position marked
- **current-buffer**: Uses only the current buffer as context, with cursor position marked
- **none**: Uses no buffer context

## Development

Run `make check` to validate parentheses, load each file, byte-compile, and lint the Elisp sources. Use `make help` to see all available maintenance targets.

## Backend (gpt.py)

The Emacs package uses a Python backend script (`gpt.py`) to handle API calls. This script can also be used standalone:

```bash
# If using from source with virtual environment
.venv/bin/python gpt.py <api_key> <model> <max_tokens> <temperature> <api_type> <prompt_file>

# Or with system Python
python3 gpt.py <api_key> <model> <max_tokens> <temperature> <api_type> <prompt_file>
```

### Backend Features

- Specific exception handling for each API provider
- Conversation format parsing (`User:` / `Assistant:` blocks)
- Automatic logging to JSONL format
- Robust error handling and validation
- Streaming output support
- OpenAI reasoning summaries

## Troubleshooting

### "No such file or directory, python"

If you see this error, it means your system doesn't have a `python` command available. This is common on modern macOS systems. Solutions:

1. **Automatic detection** (recommended): The package should automatically detect `python3`. If it doesn't, restart Emacs.
2. **Manual configuration**: Set the Python path explicitly:
   ```elisp
   (setq gpt-python-path "python3")
   ```
3. **Virtual environment**: If installing from source, ensure you've run `uv sync` and the package will use `.venv/bin/python`.

### "externally-managed-environment" Error

This is a modern Python safety feature. Use one of these solutions:

1. **Use uv** (recommended):
   ```bash
   uv sync
   ```
2. **Create a virtual environment**:
   ```bash
   python3 -m venv .venv
   source .venv/bin/activate
   pip install -r requirements.txt
   ```

### Python Package Import Errors

Ensure you have the required packages installed in the Python environment that gpt.el is using:

```bash
# Check what Python gpt.el is using
# In Emacs: M-x eval-expression RET gpt-python-path

# Then test the packages
/path/to/your/python -c "import anthropic, openai, google.genai, jsonlines"
```

## License

MIT License - see LICENSE.md for details.
