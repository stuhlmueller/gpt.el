;;; gpt-core.el --- Core functionality for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 1.4
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This file contains core variables and basic utilities for gpt.el.

;;; Code:

(require 'savehist)

(savehist-mode 1)

(defvar gpt-command-history nil
  "A list of GPT commands that have been entered by the user.")

(defvar gpt-script-path (expand-file-name "gpt.py" (file-name-directory (or load-file-name buffer-file-name)))
  "The path to the Python script used by gpt.el.")

(defvar gpt-api-type 'openai
  "The type of API to use. Either 'openai, 'anthropic, or 'google.")

(defvar gpt-model "claude-3-7-sonnet-latest"
  "The model to use (e.g., 'gpt-4o', 'claude-3-5-sonnet-latest').")

(defvar gpt-max-tokens "2000"
  "The max_tokens value used with the chosen model.")

(defvar gpt-temperature "0"
  "The temperature value used with the chosen model.")

(defvar gpt-openai-key "NOT SET"
  "The OpenAI API key to use.")

(defvar gpt-anthropic-key "NOT SET"
  "The Anthropic API key to use.")

(defvar gpt-google-key "NOT SET"
  "The Google Gemini API key to use.")

(defvar gpt-python-path "python"
  "The path to your python executable.")

(defvar gpt-use-named-buffers t
  "If non-nil, use named buffers for GPT output. Otherwise, use temporary buffers.")

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

(provide 'gpt-core)
;;; gpt-core.el ends here 