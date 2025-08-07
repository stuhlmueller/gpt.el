;;; gpt-core.el --- Core functionality for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 2.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; This file contains core variables and basic utilities for gpt.el.

;;; Code:

(require 'savehist)

(savehist-mode 1)

(defgroup gpt nil
  "Interface to instruction-following language models."
  :group 'external
  :prefix "gpt-")

(defcustom gpt-api-type 'anthropic
  "The type of API to use.  Either \\='openai, \\='anthropic, or \\='google."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Anthropic" anthropic)
                 (const :tag "Google" google))
  :group 'gpt)

(defcustom gpt-model "claude-opus-4-0"
  "The model to use (e.g., \\='gpt-4.1\\=', \\='claude-sonnet-4-0\\=')."
  :type 'string
  :group 'gpt)

(defcustom gpt-max-tokens "64000"
  "The max_tokens value used with the chosen model."
  :type 'string
  :group 'gpt)

(defcustom gpt-temperature "0"
  "The temperature value used with the chosen model."
  :type 'string
  :group 'gpt)

(defcustom gpt-available-models
  '(("GPT-4.1" . (openai . "gpt-4.1"))
    ("GPT-4.5" . (openai . "gpt-4.5-preview"))
    ("GPT-5" . (openai . "gpt-5"))
    ("o3" . (openai . "o3"))
    ("o3-pro" . (openai . "o3-pro"))
    ("o4-mini" . (openai . "o4-mini"))
    ("Claude 3.7 Sonnet" . (anthropic . "claude-3-7-sonnet-latest"))
    ("Claude 4 Sonnet" . (anthropic . "claude-sonnet-4-0"))
    ("Claude 4.1 Opus" . (anthropic . "claude-opus-4-1-20250805"))
    ("Gemini 2.5 Pro Preview" . (google . "gemini-2.5-pro-preview-06-05")))
  "Available models for GPT commands.
Each entry is a cons cell where the car is the display name and
the cdr is a cons cell of (API-TYPE . MODEL-ID)."
  :type '(alist :key-type string
                :value-type (cons (choice (const openai)
                                          (const anthropic)
                                          (const google))
                                  string))
  :group 'gpt)

  ;; Model-specific max tokens
(defcustom gpt-model-max-tokens
  '(("claude-3-7-sonnet-latest" . "64000")
    ("claude-sonnet-4-0" . "64000")
    ("claude-opus-4-0" . "32000")
    ("claude-opus-4-1-20250805" . "32000")    
    ("claude-3-5-sonnet-latest" . "8192")
    ("claude-3-5-haiku-latest" . "8192")
    ("claude-3-opus-latest" . "4096")
    ("gpt-4.1" . "128000")
    ("gpt-4.5-preview" . "128000")
    ("gpt-5" . "400000")
    ("o3" . "100000")
    ("o3-pro" . "100000")
    ("o4-mini" . "16000")
    ("gemini-2.5-pro-preview-06-05" . "8192"))
  "Maximum output tokens for each model."
  :type '(alist :key-type string :value-type string)
  :group 'gpt)

(defcustom gpt-openai-key "NOT SET"
  "The OpenAI API key to use."
  :type 'string
  :group 'gpt)

(defcustom gpt-anthropic-key "NOT SET"
  "The Anthropic API key to use."
  :type 'string
  :group 'gpt)

(defcustom gpt-google-key "NOT SET"
  "The Google Gemini API key to use."
  :type 'string
  :group 'gpt)

(defcustom gpt-thinking-enabled t
  "Enable extended thinking mode for Anthropic models."
  :type 'boolean
  :group 'gpt)

(defcustom gpt-interleaved-thinking t
  "Enable interleaved thinking with tools for Anthropic models."
  :type 'boolean
  :group 'gpt)

(defcustom gpt-web-search t
  "Enable web search for models that support it."
  :type 'boolean
  :group 'gpt)

(defvar gpt-thinking-budget "21333"
  "Token budget for extended thinking mode.
Automatically set to 1/3 of max tokens.")

(defcustom gpt-python-path
  (let* ((script-dir (when (or load-file-name buffer-file-name)
                       (file-name-directory (or load-file-name buffer-file-name))))
         (venv-python (when script-dir
                        (expand-file-name ".venv/bin/python" script-dir))))
    (if (and venv-python (file-exists-p venv-python))
        venv-python
      (or (executable-find "python3")
          (executable-find "python")
          "python3")))
  "The path to your python executable."
  :type 'string
  :group 'gpt)

(defcustom gpt-use-named-buffers t
  "If non-nil, use named buffers for GPT output.  Otherwise, use temporary buffers."
  :type 'boolean
  :group 'gpt)

(defcustom gpt-use-markdown-mode t
  "Whether to use markdown-mode features when available.
When non-nil and markdown-mode is installed, gpt-mode will
inherit markdown syntax highlighting and features."
  :type 'boolean
  :group 'gpt)

(defvar gpt-command-history nil
  "A list of GPT commands that have been entered by the user.")

(defvar gpt-script-path (expand-file-name "gpt.py" (file-name-directory (or load-file-name buffer-file-name)))
  "The path to the Python script used by gpt.el.")

(add-to-list 'savehist-additional-variables 'gpt-command-history)

(defun gpt-display-command-history ()
  "Display the `gpt-command-history' in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*GPT Command History*")
    (erase-buffer)
    (insert (mapconcat #'identity gpt-command-history "\n"))
    (switch-to-buffer (current-buffer))))

(defun gpt-clear-command-history ()
  "Clear the `gpt-command-history' list."
  (interactive)
  (setq gpt-command-history nil)
  (message "GPT command history cleared."))

(defun gpt-export-history (file)
  "Export the `gpt-command-history' to FILE."
  (interactive "Export gpt-command-history to file: ")
  (with-temp-file file
    (dolist (cmd gpt-command-history)
      (insert (format "%s\n" cmd)))))

(defun gpt-read-command-with-space (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer with completion, treating space literally.

The arguments are the same as for `completing-read', except that
space does not trigger completion or cycling, but inserts a space
character.  PROMPT is the prompt to display, COLLECTION is the
list of possible completions, and the optional arguments PREDICATE
REQUIRE-MATCH INITIAL-INPUT HIST DEF and INHERIT-INPUT-METHOD
have the same meaning as for `completing-read'."
  (let ((minibuffer-local-completion-map
         (let ((map (copy-keymap minibuffer-local-completion-map)))
           (define-key map " " 'self-insert-command)
           map)))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))

(defun gpt-update-model-settings ()
  "Update max_tokens and thinking_budget based on the current model."
  (let* ((model-max (cdr (assoc gpt-model gpt-model-max-tokens)))
         (max-tokens (or model-max "64000"))  ; Default to 64000 if model not found
         (max-tokens-num (string-to-number max-tokens))
         (thinking-budget-num (/ max-tokens-num 3))  ; 1/3 of max tokens
         (thinking-budget (number-to-string thinking-budget-num)))
    (setq gpt-max-tokens max-tokens)
    (setq gpt-thinking-budget thinking-budget)
    (message "Model settings updated: max_tokens=%s, thinking_budget=%s"
             max-tokens thinking-budget)))

;; Initialize settings on load
(gpt-update-model-settings)

(provide 'gpt-core)
;;; gpt-core.el ends here
