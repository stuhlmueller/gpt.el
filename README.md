<p align="center">
    <img src="resources/logo.png" width="20%" alt="weird-generated-logo"/>
</p>

# le-gpt.el
[![MELPA](https://melpa.org/packages/le-gpt-badge.svg)](https://melpa.org/#/le-gpt)

le-gpt.el is a comprehensive Emacs package for interacting with large language models like GPT-4 and Claude 3.5 Sonnet. It's a feature-rich fork of [gpt.el](https://github.com/stuhlmueller/gpt.el) that adds project awareness, completion, region transform, and more to come.

The aim is to make sure Emacs stays up-to-date with modern GPT support, essentially aiming for a CursorAI for Emacs.

## Features

- **Chat Interface**: Create and manage multiple chat sessions with GPT. Use `M-x le-gpt-chat` to start a session. Key bindings in chat buffers include:
  - `C-c C-c`: Send follow-up command
  - `C-c C-p`: Toggle prefix visibility
  - `C-c C-b`: Copy code block at point
  - `C-c C-t`: Generate descriptive buffer name from its content

- **Buffer List**: Display a list of all GPT chat buffers with `M-x le-gpt-list-buffers`. 
This feature allows you to manage and navigate through your GPT-related buffers efficiently.

- **Completion at Point**: Let GPT complete what you're currently writing. Use `M-x le-gpt-complete-at-point` to get suggestions based on your current cursor position.

- **Region Transformation**: Select a region you want GPT to transform. Use `M-x le-gpt-transform-region` to transform the selected region using GPT.

- **Project Context**: Select files from your project that GPT should use as context. 
Globally select project files to be used as context via `M-x le-gpt-select-project-files` or select local, per-command context by running the above commands with a prefix argument (`C-u`). Context is used by chat, completion, and region transforms. 
To deselect global context files, use `M-x le-gpt-deselect-project-files` or `M-x le-gpt-clear-selected-context-files` to clear the entire selection.

### Mandatory GIFs

| Chat Interface                                                                   | Completion at point                                                        |
|----------------------------------------------------------------------------------|----------------------------------------------------------------------------|
| ![le-gpt-chat-demo](./resources/le-gpt-chat.gif)                                 | ![le-gpt-complete-at-point-demo](./resources/le-gpt-complete-at-point.gif) |


| Project Context                                                                               | Region Transformation                                             |
|-----------------------------------------------------------------------------------------------|-------------------------------------------------------------------|
| ![le-gpt-with-context-demo](./resources/le-gpt-project-context.gif)                           | ![le-gpt-transform-region-demo](./resources/le-gpt-transform.gif) |


## Installation

### Prerequisites

You'll need Python packages for the API clients:

```bash
pip install openai anthropic jsonlines
```
You don't need to install all of them, but minimally `openai` or `anthropic`.

You'll also need API keys from [OpenAI](https://beta.openai.com/) and/or [Anthropic](https://console.anthropic.com).

You'll also need [markdown-mode](https://github.com/jrblevin/markdown-mode) for displaying the chat conversations nicely.

### Using Melpa
`le-gpt` is available via [MELPA](https://melpa.org/). 

Here's how to install it with [straight](https://github.com/radian-software/straight.el):

```elisp
(use-package le-gpt
  :bind (("M-C-g" . le-gpt-chat)
         ("M-C-n" . le-gpt-complete-at-point)
         ("M-C-t" . le-gpt-transform-region)
         ("M-C-s" . le-gpt-select-project-files)
         ("M-C-d" . le-gpt-deselect-project-files))
  :config
  ;; you need to set at least one of the following
  (setq le-gpt-openai-key "your-openai-key-here")
  (setq le-gpt-anthropic-key "your-anthropic-key-here"))
```

## Configuration

See all available customizations via `M-x customize-group RET le-gpt`.

Basic configuration:
```elisp
;; API Keys
(setq le-gpt-openai-key "sk-...")
(setq le-gpt-anthropic-key "sk-ant-...")

;; Model Parameters (optional)
(setq le-gpt-model "gpt-4o")
(setq le-gpt-max-tokens 2000)
(setq le-gpt-temperature 0)

;; API Selection (default is 'openai)
(setq le-gpt-api-type 'anthropic)
```

## Usage

### Chat Interface

Start a chat session:
```elisp
M-x le-gpt-chat
```

If you provide a prefix argument, you can select context files for a single query.

### Completion at Point

Get completions based on your current cursor position:
```elisp
M-x le-gpt-complete-at-point
```

### Project Context

Set project files as context:
```elisp
M-x le-gpt-select-project-files
```
The context will be used by chat, completion, and region transforms.

*Note that these files are persisted between multiple calls.*

To deselect files:
```elisp
M-x le-gpt-deselect-project-files
```
Or, to clear the entire selection:
```elisp
M-x le-gpt-clear-selected-context-files
```

### Region Transformation

Transform selection via:
```elisp
M-x le-gpt-transform-region
```

### Buffer List

Display a list of all GPT buffers:
```elisp
M-x le-gpt-list-buffers
```

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests on GitHub.

### Feature roadmap

 - [ ] More models, e.g., groq
 - [ ] Ability to generate images (?)
 - [ ] Add all files of the current project as context (?)
 - [ ] Ability to let GPT decide which context files it needs
 - [ ] RAG for indexing files (?)

## License

le-gpt.el is licensed under the MIT License. See [LICENSE](LICENSE) for details.

