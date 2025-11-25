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
  '(("GPT-5.1" . (openai . "gpt-5.1"))
    ("GPT-5 Mini" . (openai . "gpt-5-mini"))
    ("GPT-5 Nano" . (openai . "gpt-5-nano"))
    ("Claude 4.5 Opus" . (anthropic . "claude-opus-4-5"))
    ("Claude 4.5 Sonnet" . (anthropic . "claude-sonnet-4-5"))
    ("Gemini 3 Pro (Preview)" . (google . "gemini-3-pro-preview")))
  "Available models for GPT commands.
Each entry is a cons cell where the car is the display name and
the cdr is a cons cell of (API-TYPE . MODEL-ID)."
  :type '(alist :key-type string
                :value-type (cons (choice (const openai)
                                          (const anthropic)
                                          (const google))
                                  string))
  :group 'gpt)

;; Default models for multi-model command
(defcustom gpt-multi-models-default '("GPT-5.1" "Claude 4.5 Opus" "Gemini 3 Pro (Preview)")
  "Models used by `gpt-chat-multi-models'.
Use a prefix argument (C-u) to pick models interactively."
  :type '(repeat (string :tag "Model name (display label)"))
  :group 'gpt)

  ;; Model-specific max tokens
(defcustom gpt-model-max-tokens
  '(("claude-opus-4-5" . "32000")
    ("claude-sonnet-4-5" . "64000")
    ("gpt-5.1" . "400000")
    ("gpt-5-mini" . "200000")
    ("gpt-5-nano" . "100000")
    ("gemini-3-pro-preview" . "60000"))
  "Maximum output tokens for each model."
  :type '(alist :key-type string :value-type string)
  :group 'gpt)

(defun gpt-update-model-settings ()
  "Update max_tokens and thinking_budget based on the current model."
  (let* ((model-max (cdr (assoc gpt-model gpt-model-max-tokens)))
         (max-tokens (or model-max "64000"))  ; Default to 64000 if model not found
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
