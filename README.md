<p align="center">
  <img src="gpt.gif" alt="gpt.el demo" width="600"/>
</p>

# gpt.el

gpt.el is a simple Emacs package that lets you interact with instruction-following language models like GPT-4 and Claude 3.5 Sonnet from your editor. You can type a natural language command (with history and completion support) and optionally use the current region or buffer contents as input for the model. The package displays the output of the model in a temporary or named buffer, and updates it as the model generates more text. You can issue follow-up commands that provide the interaction history in that buffer as context. You can also browse, save, and clear the command history for later reference.

## Installation

To use gpt.el, you need to have at least one of the `openai` and `anthropic` Python packges as well as `jsonlines`. You'll also need valid API keys for OpenAI or Anthropic. You can install the packages with:

```
pip install openai anthropic jsonlines
```

You can get an OpenAI API key from https://beta.openai.com/ and an Anthropic API key from https://console.anthropic.com.

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

You need to set the variables `gpt-openai-key` and/or `gpt-anthropic-key` to your respective API keys to use gpt.el. For example:

```elisp
(setq gpt-openai-key "sk-Aes.....AV8qzL")
(setq gpt-anthropic-key "sk-ant-api03-...")
```

Optionally, you can customize the model parameters by setting the variables `gpt-model`, `gpt-max-tokens`, and `gpt-temperature`. The defaults are:

```elisp
(setq gpt-model "gpt-4o")
(setq gpt-max-tokens 2000)
(setq gpt-temperature 0)
```

By default, gpt.el uses the OpenAI API. To switch to the Anthropic API, you can set:

```elisp
(setq gpt-api-type 'anthropic)
```

## Usage

### Running commands

To run a generative model command, use the `gpt-dwim` function.

`gpt-dwim` prompts you to choose a context mode:

- "all-buffers": Uses all visible buffers as context
- "current-buffer": Uses only the current buffer as context
- "none": Uses no buffer context

Alternatively you can use one of the following wrapper functions:

1. `gpt-dwim-all-buffers`: Directly uses all visible buffers as context
2. `gpt-dwim-current-buffer`: Directly uses the current buffer as context
3. `gpt-dwim-no-context`: Uses no buffer context

You can bind these functions to keys of your choice, for example:

```elisp
(global-set-key (kbd "M-C-g") 'gpt-dwim)
(global-set-key (kbd "M-C-b") 'gpt-dwim-all-buffers)
```

When you invoke any of these commands, you'll see a prompt that shows what context will be included. For example:

```
GPT [all buffers + selection]: 
```

The command you entercan be any text. For example:

```
Write a haiku about Emacs.
```

If you have an active region, it will always be included as additional context. If you enter "n/a" as the command, only the context will be passed to the model.

The output will be displayed in a temporary or named buffer, with the same major mode as the original buffer. The output will be streamed as it is produced by the model. You can switch back to the original buffer at any time.

### Buffer Display

By default, output is shown in a buffer named after the first few characters of your command. You have two options for buffer naming:

1. Automatic naming: The buffer name will be truncated to `gpt-buffer-name-length` characters
2. GPT-generated naming: Use `C-c C-t` (`gpt-generate-buffer-name`) to have GPT generate a meaningful name based on your command

### Markdown Support

The output buffer uses markdown-mode if available, falling back to text-mode if not. This provides better syntax highlighting for code blocks and other markdown elements.

### Buffer Commands

In the gpt-output buffer:
- `C-c C-c`: Run a follow-up command
- `C-c C-b`: Copy the content of the code block at point
- `C-c C-p`: Toggle visibility of "User:", "Human:", and "Assistant:" prefixes
- `C-c C-m`: Switch between different models
- `C-c C-t`: Generate a meaningful buffer name using GPT

### Follow-up Commands

In the gpt-output buffer, `C-c C-c` is bound to running a follow-up command that is provided the previous commands and outputs as input. For example, you can run a command "Explain this in more detail" to get more information about the previous response.

### History

You can view the command history by calling `gpt-display-command-history`, which will show the commands in a buffer. You can also export the command history to a file by calling `gpt-export-history`, which will prompt you for a file name. To clear the command history, use the `gpt-clear-command-history` function.

## Contributing

Contributions to gpt.el are welcome! Please feel free to submit pull requests or create issues for bugs and feature requests.

## License

gpt.el is licensed under the MIT License. See [LICENSE.md](LICENSE.md) for details.
