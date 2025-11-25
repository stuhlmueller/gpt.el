;;; gpt-core.el --- Core functionality for gpt.el -*- lexical-binding: t; package-lint-main-file: "gpt.el"; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 2.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains core variables and basic utilities for gpt.el.

;;; Code:

(require 'savehist)
(require 'cl-lib)

(defvar gpt-max-tokens "64000")
(defvar gpt-thinking-budget "21333")

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

(defcustom gpt-available-models
  '(("GPT-5.1" . (:api openai :id "gpt-5.1" :max-tokens "400000"))
    ("GPT-5 Mini" . (:api openai :id "gpt-5-mini" :max-tokens "200000"))
    ("GPT-5 Nano" . (:api openai :id "gpt-5-nano" :max-tokens "100000"))
    ("Claude 4.5 Opus" . (:api anthropic :id "claude-opus-4-5" :max-tokens "32000"))
    ("Claude 4.5 Sonnet" . (:api anthropic :id "claude-sonnet-4-5" :max-tokens "64000"))
    ("Gemini 3 Pro (Preview)" . (:api google :id "gemini-3-pro-preview" :max-tokens "60000")))
  "Available models for GPT commands.
Each entry is (DISPLAY-NAME . PLIST) where PLIST contains:
  :api        - API provider symbol (openai, anthropic, google)
  :id         - Model ID string for the API
  :max-tokens - Maximum output tokens as string

This is the single source of truth for model definitions."
  :type '(alist :key-type string :value-type plist)
  :group 'gpt)

;; Default models for multi-model command
(defcustom gpt-multi-models-default '("GPT-5.1" "Claude 4.5 Opus" "Gemini 3 Pro (Preview)")
  "Models used by `gpt-chat-multi-models'.
Use a prefix argument (C-u) to pick models interactively.
Model names must match keys in `gpt-available-models'."
  :type '(repeat (string :tag "Model name (display label)"))
  :group 'gpt)

(defun gpt--model-max-tokens (model-id)
  "Return max tokens for MODEL-ID from `gpt-available-models', or nil."
  (cl-loop for (_name . plist) in gpt-available-models
           when (equal (plist-get plist :id) model-id)
           return (plist-get plist :max-tokens)))

(defun gpt-update-model-settings ()
  "Update max_tokens and thinking_budget based on the current model."
  (let* ((max-tokens (or (gpt--model-max-tokens gpt-model) "64000"))  ; Default if model not found
         (max-tokens-num (string-to-number max-tokens))
         (thinking-budget-num (/ max-tokens-num 3))  ; 1/3 of max tokens
         (thinking-budget (number-to-string thinking-budget-num)))
    (setq gpt-max-tokens max-tokens)
    (setq gpt-thinking-budget thinking-budget)
    ;; Avoid noisy messages during package load; keep variables in sync silently.
    ))

(defun gpt--set-model (symbol value)
  "Set SYMBOL to VALUE and refresh derived settings."
  (set-default symbol value)
  (let ((gpt-model value))
    (gpt-update-model-settings)))

(defun gpt--model-watcher (symbol newval operation _where)
  "Keep derived settings in sync when SYMBOL is changed.
NEWVAL is the new value and OPERATION is the kind of change (set/let)."
  (when (and (eq symbol 'gpt-model)
             (memq operation '(set let)))
    (let ((gpt-model newval))
      (gpt-update-model-settings))))

(defcustom gpt-model "claude-opus-4-5"
  "The model to use (e.g., \\='gpt-4.1\\=', \\='claude-opus-4-5\\=')."
  :type 'string
  :set #'gpt--set-model
  :group 'gpt)

(defcustom gpt-max-tokens "64000"
  "The max_tokens value used with the chosen model."
  :type 'string
  :group 'gpt)

(defcustom gpt-temperature "0"
  "The temperature value used with the chosen model."
  :type 'string
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
  "Enable extended thinking mode for Anthropic models.

Constraints when enabled:
- Temperature must be 1.0 (automatically enforced by the API)
- Thinking budget must be less than max_tokens
- Interleaved thinking and 1M context beta are mutually exclusive

Extended thinking allows the model to reason step-by-step before
responding, which can improve quality for complex tasks."
  :type 'boolean
  :group 'gpt)

(defcustom gpt-interleaved-thinking t
  "Enable interleaved thinking with tools for Anthropic models.

When enabled, thinking blocks are streamed as they occur, showing
the model's reasoning process in real-time.

Note: Interleaved thinking is mutually exclusive with the 1M context
window beta.  When interleaved thinking is enabled, the context window
is limited to the standard size."
  :type 'boolean
  :group 'gpt)

(defcustom gpt-web-search t
  "Enable web search for models that support it.

For Anthropic models, this uses the built-in web search tool to
ground responses with current information from the web.

Note: Web search adds latency but improves accuracy for questions
about recent events or facts that may have changed since training."
  :type 'boolean
  :group 'gpt)

;; OpenAI reasoning settings for GPT-5 family
(defcustom gpt-openai-reasoning-effort "medium"
  "Reasoning effort for OpenAI GPT-5 family models: low, medium, or high."
  :type '(choice (const "low") (const "medium") (const "high"))
  :group 'gpt)

(defcustom gpt-openai-reasoning-summary "detailed"
  "Reasoning summary for OpenAI GPT-5 family models.
Use nil, auto, concise, or detailed."
  :type '(choice (const nil)
                 (const "auto")
                 (const "concise")
                 (const "detailed"))
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

(defun gpt-validate-api-key ()
  "Check that the API key for the current `gpt-api-type' is configured.
Signals a `user-error' with a helpful message if the key is not set."
  (let ((api-key (cond ((eq gpt-api-type 'openai) gpt-openai-key)
                       ((eq gpt-api-type 'anthropic) gpt-anthropic-key)
                       ((eq gpt-api-type 'google) gpt-google-key)
                       (t "NOT SET")))
        (key-var (cond ((eq gpt-api-type 'openai) "gpt-openai-key")
                       ((eq gpt-api-type 'anthropic) "gpt-anthropic-key")
                       ((eq gpt-api-type 'google) "gpt-google-key"))))
    (when (or (null api-key) (string= api-key "NOT SET") (string-empty-p api-key))
      (user-error "API key for %s is not set. Please configure `%s'"
                  (symbol-name gpt-api-type) key-var))))

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

;; Initialize settings on load
(gpt-update-model-settings)

;; Keep derived settings in sync when gpt-model changes via setq/customize.
(defvar gpt--model-watcher-installed nil
  "Whether the gpt-model watcher has been installed.")
(when (and (fboundp 'add-variable-watcher)
           (not gpt--model-watcher-installed))
  (add-variable-watcher 'gpt-model #'gpt--model-watcher)
  (setq gpt--model-watcher-installed t))

(provide 'gpt-core)
;;; gpt-core.el ends here
