;;; le-gpt.el --- Emacs on steroids with GPT -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Andreas Stuhlmueller, 2024- Anselm Coogan

;; Author: Andreas Stuhlmueller <andreas@ought.org>
;; Maintainer: Anselm Coogan <anselm.coogan@gmail.com>
;; Version: 0.3.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/AnselmC/le-gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "28.1") (markdown-mode "2.6"))

;;; Commentary:

;; This package is a fork of https://github.com/stuhlmueller/gpt.el
;; It aims to provide more comprehensive GPT integration in Emacs than its parent.
;; Features include:
;; - Multiple chats with different models
;; - Completion at point
;; - Region transformations
;; - Project context awareness

;;; Code:

(require 'markdown-mode)

(require 'le-gpt-core)
(require 'le-gpt-chat)
(require 'le-gpt-transform)
(require 'le-gpt-completion)
(require 'le-gpt-project)

;;;###autoload
(defun le-gpt-chat (&optional temp-context-files)
  "Start a GPT chat session.
With prefix argument TEMP-CONTEXT-FILES,
interactively select context files to be used for this command."
  (interactive "P")
  (le-gpt-chat-start temp-context-files))

;;;###autoload
(defun le-gpt-transform-region (&optional temp-context-files)
  "Transform the selected region using GPT.
With prefix argument TEMP-CONTEXT-FILES,
interactively select context files to be used for this command."
  (interactive "P")
  (le-gpt-transform-region-with-prompt temp-context-files))

;;;###autoload
(defun le-gpt-complete-at-point (&optional temp-context-files)
  "Get completion suggestions from GPT at point.
With prefix argument TEMP-CONTEXT-FILES,
interactively select context files to be used for this command."
  (interactive "P")
  (le-gpt-completion-at-point temp-context-files))

;;;###autoload
(defun le-gpt-select-project-files ()
  "Prompt user to select files from project to use as context."
  (interactive)
  (le-gpt-select-project-files-for-context))

;;;###autoload
(defun le-gpt-deselect-project-files ()
  "Remove multiple files from the current project context."
  (interactive)
  (le-gpt-deselect-project-files-for-context))

(defgroup le-gpt nil
  "Customization group for Le GPT."
  :group 'applications)

(provide 'le-gpt)

;;; le-gpt.el ends here
