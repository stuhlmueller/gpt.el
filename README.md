<p align="center">
  <img src="gpt.gif" alt="gpt.el demo" width="600"/>
</p>

# gpt.el

gpt.el is a simple Emacs package that lets you interact with instruction-following language models like GPT-4.1, Claude 3.7 Sonnet, and Google Gemini from your editor. You can type a natural language command (with history and completion support) and optionally use the current region or buffer contents as input for the model. The package displays the output of the model in a temporary or named buffer, and updates it as the model generates more text. You can issue follow-up commands that provide the interaction history in that buffer as context. You can also browse, save, and clear the command history for later reference.

## Installation

To use gpt.el, you need to have at least one of the `openai`, `anthropic`, or `google-genai` Python packages as well as `jsonlines`. You'll also need valid API keys for OpenAI, Anthropic, or Google. You can install the packages with:

```
pip install openai anthropic google-genai jsonlines
```

You can get an OpenAI API key from https://beta.openai.com/, an Anthropic API key from https://console.anthropic.com, and a Google Gemini API key from https://aistudio.google.com/app/apikey.

### From MELPA package repository

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

Once installed, you can require the package in your init file:

```elisp
(require 'gpt)
```

Alternatively, you can use `use-package`:

```elisp
(use-package gpt)
```

### From source

To install gpt.el, clone this repository and add the following to your Emacs init file:

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

You need to set at least one of the variables `gpt-openai-key`, `gpt-anthropic-key`, or `gpt-google-key` to your respective API keys to use gpt.el. For example:

```elisp
(setq gpt-openai-key "sk-Aes.....AV8qzL")
(setq gpt-anthropic-key "sk-ant-api03-...")
(setq gpt-google-key "AIzaSy...")
```

Optionally, you can customize the model parameters by setting the variables `gpt-model`, `gpt-max-tokens`, and `gpt-temperature`. The defaults are:

```elisp
(setq gpt-model "claude-3-7-sonnet-latest")
(setq gpt-max-tokens 2000)
(setq gpt-temperature 0)
```

By default, gpt.el uses the OpenAI API. To switch to the Anthropic or Google Gemini API, you can set:

```elisp
;; For Anthropic
(setq gpt-api-type 'anthropic)

;; For Google Gemini
(setq gpt-api-type 'google)
```

## Usage

### Running commands

To run a generative model command, use the `gpt-chat` function.

`gpt-chat` prompts you to choose a context mode:

- "all-buffers": Uses all visible buffers as context, with cursor position marked
- "current-buffer": Uses only the current buffer as context, with cursor position marked
- "none": Uses no buffer context

Alternatively you can use one of the following wrapper functions:

1. `gpt-chat-all-buffers`: Directly uses all visible buffers as context
2. `gpt-chat-current-buffer`: Directly uses the current buffer as context
3. `gpt-chat-no-context`: Uses no buffer context

You can bind these functions to keys of your choice, for example:

```elisp
(global-set-key (kbd "M-C-g") 'gpt-chat)
(global-set-key (kbd "M-C-b") 'gpt-chat-all-buffers)
```
