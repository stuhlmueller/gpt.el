<p align="center">
  <img src="gpt.gif" alt="gpt.el demo" width="600"/>
</p>

# gpt.el

gpt.el is an Emacs package for calling instruction-following language models such as GPT-3 Instruct (`text-davinci-002`). You enter a command (with history and completion), and optionally use the current region as input. The output of the command is displayed in a temporary buffer with the same major mode as the original buffer. The output is streamed as it is produced by the generative model. You can also view and export the command history to a file.

## Installation

To use gpt.el, you need to have the `openai` Python package installed and a valid OpenAI API key. You can install the package with `pip install openai`, and get an API key from https://beta.openai.com/.

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

You need to set the variable `gpt-openai-key` to your OpenAI API key use gpt.el. For example:

```elisp
(setq gpt-openai-key "sk-Aes.....AV8qzL")
```

You can also customize the engine  by setting the variable `gpt-openai-engine`:

```elisp
(setq gpt-openai-engine "code-davinci-002")
```

By default, it is set to `text-davinci-002`.

## Usage

To run a generative model command, use the `gpt-dwim` function. You can bind it to a key of your choice, for example:

```elisp
(global-set-key (kbd "M-C-g") 'gpt-dwim)
```

When you invoke `gpt-dwim`, you will be prompted for a command, with history and completion. The command can be any text. For example:

```
Write a haiku about Emacs.
```

If you have an active region, it will be used as contextual input to the command. The output of GPT running the command will be displayed in a temporary buffer, with the same major mode as the original buffer. The output will be streamed as it is produced by the generative model. You can switch back to the original buffer at any time.

You can view the command history by calling `display-gpt-command-history`, which will show the commands in a buffer. You can also export the command history to a file by calling `gpt-export-history`, which will prompt you for a file name.

## License

gpt.el is licensed under the MIT License. See LICENSE for details.
