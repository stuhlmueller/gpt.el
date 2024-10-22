<p align="center">
  <img src="gpt.gif" alt="gpt.el demo" width="600"/>
</p>

# gpt.el

gpt.el is a simple Emacs package that lets you interact with instruction-following language models like GPT-4 and Claude 3.5 Sonnet from your editor. 
You can either chat with a model or have it complete your text at-point.
You start a chat by typing a natural language command (with history and completion support) and optionally use the current region or all visible buffers as input for the model. The chat is created in a temporary or named buffer, and is updated as the model generates more text. You can issue follow-up commands that provide the interaction history in that buffer as context. You can also browse, save, and clear the command history for later reference.
To complete at point, the model takes the buffer up to point as the text it should complete and the rest of the buffer content as context.

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

### Chat

To run a generative model command, use the `gpt-dwim` function. You can bind it to a key of your choice, for example:

```elisp
(global-set-key (kbd "M-C-g") 'gpt-dwim)
```

When you invoke `gpt-dwim`, you will be prompted for a command, with history and completion. The command can be any text. For example:

```
Write a haiku about Emacs.
```

If you have an active region, it will be used as contextual input to the command. If you enter n/a as the command, only the region will be passed to the model. The output of the model running the command will be displayed in a temporary buffer, with the same major mode as the original buffer. The output will be streamed as it is produced by the generative model. You can switch back to the original buffer at any time.

If you want to use all visible buffers as input, use the `gpt-dwim-all-buffers` function. You can bind it to a key of your choice, for example:

```elisp
(global-set-key (kbd "M-C-b") 'gpt-dwim-all-buffers)
```

#### Follow-up commands

In the gpt-output buffer, `C-c C-c` is bound to running a follow-up command that is provided the previous commands and outputs as input. For example, you can run a command "Explain this in more detail" to get more information about the previous response.

#### Copying code blocks

In the gpt-output buffer, `C-c C-b` is bound to copying the content of the code block at point to the clipboard.

#### Toggling prefix visibility

In the gpt-output buffer, `C-c C-p` is bound to toggling the visibility of the "User:", "Human:", and "Assistant:" prefixes.

#### History

You can view the command history by calling `gpt-display-command-history`, which will show the commands in a buffer. You can also export the command history to a file by calling `gpt-export-history`, which will prompt you for a file name. To clear the command history, use the `gpt-clear-command-history` function.

#### GPT buffer names

By default, the created buffer names will just be the first `gpt-buffer-name-length` characters of the original command. To have more meaningful names, you can have GPT generate it for you via `gpt-generate-buffer-name` (`C-c C-t`). Note that this call to GPT is synchronous. 

### Complete at point

To have the model complete what you're writing, you can use `gpt-complete-at-point`. As before, you can bind it to your prefered key, for example,

```elisp
(global-set-key (kbd "M-C-n") 'gpt-complete-at-point)
```

The model will be instructed to complete the text from the start of the buffer up to your current point. It will be given the remainder of the buffer to use as context.
The completion will be displayed incrementally using the face defined in `gpt-completion-preview-face`. Once the completion is done, hit `RET` to accept it or any other key to discard it.

### Switching models

You can switch between different models (GPT-4, GPT-3.5-turbo, and Claude 3 Sonnet) using the `gpt-switch-model` function. In the gpt-output buffer, `C-c C-m` is bound to this function. When called, it will prompt you to choose a model from the available options.

## Contributing

Contributions to gpt.el are welcome! Please feel free to submit pull requests or create issues for bugs and feature requests.

## License

gpt.el is licensed under the MIT License. See [LICENSE.md](LICENSE.md) for details.
