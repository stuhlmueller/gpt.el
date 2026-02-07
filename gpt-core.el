;;; gpt-core.el --- Core functionality for gpt.el -*- lexical-binding: t; package-lint-main-file: "gpt.el"; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 3.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains core variables and basic utilities for gpt.el.
;; This is the pure Elisp version - no Python dependencies.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'gpt-backend)

;; Forward declarations for provider-specific create functions
(declare-function gpt-openai-create "gpt-openai" (api-key))
(declare-function gpt-anthropic-create "gpt-anthropic" (api-key &rest args))
(declare-function gpt-google-create "gpt-google" (api-key))

;;; Customization group

(defgroup gpt nil
  "Interface to instruction-following language models."
  :group 'external
  :prefix "gpt-")

;;; Backend management

(defvar gpt-current-backend nil
  "The current backend instance for API calls.
This is an object of type `gpt-backend' or its subclasses.")

(defvar gpt-backends nil
  "Alist of available backends keyed by provider symbol.
Each entry is (PROVIDER . BACKEND-INSTANCE).")

(defun gpt--backend-valid-p (backend provider)
  "Return t if BACKEND is a valid instance for PROVIDER."
  (and backend
       (eieio-object-p backend)
       (object-of-class-p backend 'gpt-backend)
       (pcase provider
         ('openai (object-of-class-p backend 'gpt-openai-backend))
         ('anthropic (object-of-class-p backend 'gpt-anthropic-backend))
         ('google (object-of-class-p backend 'gpt-google-backend))
         (_ nil))))

(defun gpt-get-backend (provider)
  "Get or create backend for PROVIDER symbol.
Validates cached backends to ensure they are the correct type."
  (let ((cached (alist-get provider gpt-backends)))
    (if (gpt--backend-valid-p cached provider)
        cached
      ;; Invalid or missing - create fresh
      (let ((backend (gpt--create-backend provider)))
        (when backend
          (setf (alist-get provider gpt-backends) backend))
        backend))))

(defun gpt--create-backend (provider)
  "Create a new backend instance for PROVIDER."
  (pcase provider
    ('openai
     (require 'gpt-openai)
     (when gpt-openai-key
       (gpt-openai-create gpt-openai-key)))
    ('anthropic
     (require 'gpt-anthropic)
     (when gpt-anthropic-key
       (gpt-anthropic-create
        gpt-anthropic-key
        :thinking-enabled gpt-thinking-enabled
        :thinking-budget gpt-thinking-budget
        :interleaved-thinking gpt-interleaved-thinking
        :web-search gpt-web-search)))
    ('google
     (require 'gpt-google)
     (when gpt-google-key
       (gpt-google-create gpt-google-key)))))

(defun gpt-update-backend ()
  "Update or recreate the current backend based on settings.
Call this after changing API keys or backend settings."
  (let ((provider (gpt--current-api-type)))
    ;; Remove cached backend so it gets recreated
    (setf (alist-get provider gpt-backends) nil)
    (setq gpt-current-backend (gpt-get-backend provider))))

;;; Model definitions

(defcustom gpt-available-models
  '(("GPT-5.2" . (:api openai :id "gpt-5.2" :max-tokens 400000))
    ("GPT-5.1" . (:api openai :id "gpt-5.1" :max-tokens 400000))
    ("GPT-5 Mini" . (:api openai :id "gpt-5-mini" :max-tokens 200000))
    ("Claude 4.6 Opus" . (:api anthropic :id "claude-opus-4-6" :max-tokens 32000))
    ("Claude 4.5 Sonnet" . (:api anthropic :id "claude-sonnet-4-5" :max-tokens 64000))
    ("Gemini 3 Pro (Preview)" . (:api google :id "gemini-3-pro-preview" :max-tokens 60000)))
  "Available models for GPT commands.
Each entry is (DISPLAY-NAME . PLIST) where PLIST contains:
  :api        - API provider symbol (openai, anthropic, google)
  :id         - Model ID string for the API
  :max-tokens - Maximum output tokens as integer"
  :type '(alist :key-type string :value-type plist)
  :group 'gpt)

(defcustom gpt-multi-models-default '("GPT-5.2" "Claude 4.6 Opus" "Gemini 3 Pro (Preview)")
  "Models used by `gpt-chat-multi-models'.
Use \\[universal-argument] to pick models interactively.
Model names must match keys in `gpt-available-models'."
  :type '(repeat (string :tag "Model name (display label)"))
  :group 'gpt)

(defun gpt--model-max-tokens (model-id)
  "Return max tokens for MODEL-ID from `gpt-available-models', or nil."
  (cl-loop for (_name . plist) in gpt-available-models
           when (equal (plist-get plist :id) model-id)
           return (plist-get plist :max-tokens)))

(defun gpt--get-model-api (model-id)
  "Return API provider for MODEL-ID from `gpt-available-models', or nil."
  (cl-loop for (_name . plist) in gpt-available-models
           when (equal (plist-get plist :id) model-id)
           return (plist-get plist :api)))

(defun gpt--current-api-type ()
  "Return the API provider for the current `gpt-model'."
  (or (gpt--get-model-api gpt-model)
      'anthropic))

;;; Thinking budget (must be defined before gpt-model due to initialization order)

(defcustom gpt-thinking-budget-fraction 3
  "Fraction of max_tokens to allocate for thinking budget.
Thinking budget = max_tokens / this value."
  :type 'integer
  :group 'gpt)

(defvar gpt-thinking-budget 21333
  "Token budget for extended thinking mode.
Automatically set based on `gpt-thinking-budget-fraction'.")

;;; Settings

(defun gpt-update-model-settings ()
  "Update max_tokens and thinking_budget based on the current model."
  (let* ((max-tokens (or (gpt--model-max-tokens gpt-model) 64000))
         (api-type (gpt--get-model-api gpt-model)))
    (setq gpt-max-tokens max-tokens)
    (setq gpt-thinking-budget (/ max-tokens gpt-thinking-budget-fraction))
    ;; Update backend for new API type (only after init complete)
    (when (and api-type (featurep 'gpt-core))
      (setq gpt-current-backend (gpt-get-backend api-type)))))

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

(defcustom gpt-model "claude-opus-4-6"
  "The model to use (e.g., \\='gpt-5.2\\=', \\='claude-opus-4-6\\=')."
  :type 'string
  :set #'gpt--set-model
  :group 'gpt)

(defcustom gpt-max-tokens 64000
  "The max_tokens value used with the chosen model."
  :type 'integer
  :group 'gpt)

(defcustom gpt-temperature "0"
  "The temperature value used with the chosen model."
  :type 'string
  :group 'gpt)

;;; API Keys

(defcustom gpt-openai-key nil
  "The OpenAI API key to use."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "API key"))
  :group 'gpt)

(defcustom gpt-anthropic-key nil
  "The Anthropic API key to use."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "API key"))
  :group 'gpt)

(defcustom gpt-google-key nil
  "The Google Gemini API key to use."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "API key"))
  :group 'gpt)

;;; Anthropic-specific settings

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

;;; OpenAI-specific settings

(defcustom gpt-openai-reasoning-effort "medium"
  "Reasoning effort for OpenAI GPT-5 family models: low, medium, or high."
  :type '(choice (const "low") (const "medium") (const "high"))
  :group 'gpt)

(defcustom gpt-openai-reasoning-summary "detailed"
  "Reasoning summary for OpenAI GPT-5 family models."
  :type '(choice (const nil)
                 (const "auto")
                 (const "concise")
                 (const "detailed"))
  :group 'gpt)

;;; UI Settings

(defcustom gpt-use-named-buffers t
  "If non-nil, use named buffers for GPT output."
  :type 'boolean
  :group 'gpt)

(defcustom gpt-use-markdown-mode t
  "Whether to use markdown-mode features when available."
  :type 'boolean
  :group 'gpt)

;;; Command history

(defvar gpt-command-history nil
  "A list of GPT commands that have been entered by the user.")

(when (boundp 'savehist-additional-variables)
  (add-to-list 'savehist-additional-variables 'gpt-command-history))

;;; Validation

(defun gpt-validate-api-key ()
  "Check that the API key for the current model is configured."
  (let* ((api-type (gpt--current-api-type))
         (api-key (pcase api-type
                    ('openai gpt-openai-key)
                    ('anthropic gpt-anthropic-key)
                    ('google gpt-google-key)))
         (key-var (pcase api-type
                    ('openai "gpt-openai-key")
                    ('anthropic "gpt-anthropic-key")
                    ('google "gpt-google-key"))))
    (when (or (null api-key) (string-empty-p api-key))
      (user-error "API key for %s is not set. Please configure `%s'"
                  (symbol-name api-type) key-var))))

;;; History functions

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

;;; Utilities

(defun gpt-read-command-with-space (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer with completion, treating space literally.
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF,
and INHERIT-INPUT-METHOD have the same meaning as for `completing-read'."
  (let ((minibuffer-local-completion-map
         (let ((map (copy-keymap minibuffer-local-completion-map)))
           (define-key map " " 'self-insert-command)
           map)))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))

;;; Initialize settings on load

(gpt-update-model-settings)

;; Keep derived settings in sync when gpt-model changes
(defvar gpt--model-watcher-installed nil
  "Whether the gpt-model watcher has been installed.")

(when (and (fboundp 'add-variable-watcher)
           (not gpt--model-watcher-installed))
  (add-variable-watcher 'gpt-model #'gpt--model-watcher)
  (setq gpt--model-watcher-installed t))

(provide 'gpt-core)
;;; gpt-core.el ends here
