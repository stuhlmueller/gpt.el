# gpt-pilot.el

gpt-pilot.el is a comprehensive Emacs package for interacting with large language models like GPT-4 and Claude 3.5 Sonnet. It's a feature-rich fork of [gpt.el](https://github.com/stuhlmueller/gpt.el) that adds project awareness, completion, region transform, and more to come.

The aim is to make sure emacs stays up-to-date with modern GPT support. Essentially aiming for a CursorAI for emacs.

## Features

| Chat Interface                                         | Completion at point                                                              |
|--------------------------------------------------------|----------------------------------------------------------------------------------|
| ![gpt-pilot-chat-demo](./resources/gpt-pilot-chat.gif) | ![gpt-pilot-complete-at-point-demo](./resources/gpt-pilot-complete-at-point.gif) |
| Create and manage multiple chat sessions               | Let GPT complete what you're currently writing                                   |
| `M-x gpt-pilot-chat`                                        | `M-x gpt-pilot-complete-at-point`                                                                                  |


| Project Context                                                                   | Region Transformation                                                   |
|-----------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| ![gpt-pilot-with-context-demo](./resources/gpt-pilot-with-context.gif)            | ![gpt-pilot-transform-region-demo](./resources/gpt-pilot-transform.gif) |
| Select files from your project that GPT should use as context                     | Select a region you want to GPT to transform                            |
| `M-x gpt-pilot-select-project-files` & `M-x gpt-pilot-clear-project-file-context` | `M-x gpt-pilot-transform-region`                                        |




## Installation

### Prerequisites

You'll need Python packages for the API clients:

```bash
pip install openai anthropic jsonlines
```
You don't need to install all of them, but minimally `openai` or `anthropic`.

You'll also need API keys from [OpenAI](https://beta.openai.com/) and/or [Anthropic](https://console.anthropic.com).

### Using straight with use-package
```elisp
(use-package gpt-pilot
  :straight (gpt-pilot :type git
                       :host github
                       :repo "AnselmC/gpt-pilot.el")
  :bind (("M-C-g" . gpt-pilot-chat)
         ("M-C-n" . gpt-pilot-complete-at-point)
         ("M-C-t" . gpt-pilot-transform-region)
         ("M-C-s" . gpt-pilot-select-project-files)
         ("M-C-d" . gpt-pilot-deselect-project-files)))
  :config
  ;; you need to set at least one of the following
  (setq gpt-pilot-openai-key "your-openai-key-here")
  (setq gpt-pilot-anthropic-key "your-anthropic-key-here"))
```

## Configuration

Basic configuration:

```elisp
;; API Keys
(setq gpt-pilot-openai-key "sk-...")
(setq gpt-pilot-anthropic-key "sk-ant-...")

;; Model Parameters (optional)
(setq gpt-pilot-model "gpt-4o")
(setq gpt-pilot-max-tokens "2000")
(setq gpt-pilot-temperature "0")

;; API Selection (default is 'openai)
(setq gpt-pilot-api-type 'anthropic)
```
See all available customizations via `M-x customize-group RET gpt-pilot`.

## Usage

### Chat Interface

Start a chat session:
```elisp
M-x gpt-pilot-chat
```

Key bindings in chat buffers:
- `C-c C-c`: Send follow-up command
- `C-c C-p`: Toggle prefix visibility
- `C-c C-b`: Copy code block at point
- `C-c C-t`: Generate descriptive buffer name from its content

If you provide a prefix argument, all visible buffers will be used as additional context.

If you've set `gpt-pilot-chat-use-named-buffers` to `t`, then you can have GPT create a buffer name based on the conversation:

``` elisp
M-x gpt-pilot-chat-generate-buffer-name
```

### Completion at Point

Get completions based on your current cursor position:
```elisp
M-x gpt-pilot-complete-at-point
```


### Project Context

Set project files as context:
```elisp
M-x gpt-pilot-select-project-files
```

Note that you'll need to run `gpt-pilot-clear-project-file-context` to remove the files from subsequent requests.
The context will be used by chat, completion and region transforms.

### Region Transformation

Transform selected text:
```elisp
M-x gpt-pilot-transform-region
```


## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests on GitHub.

### Feature roadmap

 - [ ] Add testing with buttercup
 - [ ] Add testing with pytest
 - [ ] Create github actions
 - [ ] Add package to Melpa
 - [ ] Settings page
 - [ ] Add all files of current project as context (?)
 - [ ] Ability to let GPT decide which context files it needs

## License

gpt-pilot.el is licensed under the MIT License. See [LICENSE](LICENSE) for details.
