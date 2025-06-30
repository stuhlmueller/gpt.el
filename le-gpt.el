;;; le-gpt.el --- Emacs on steroids with GPT -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Andreas Stuhlmueller, 2024- Anselm Coogan

;; Author: Andreas Stuhlmueller <andreas@ought.org>
;; Maintainer: Anselm Coogan <anselm.coogan@gmail.com>
;; Version: 0.6.0
;; Keywords: openai, anthropic, deepseek, gpt, claude, language, copilot, convenience, tools, llm
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
;; - Context awareness

;;; Code:

(require 'markdown-mode)

(require 'le-gpt-core)
(require 'le-gpt-chat)   
(require 'le-gpt-transform)
(require 'le-gpt-completion)
(require 'le-gpt-context)

;;;###autoload
(defun le-gpt-chat (&optional use-context)
  "Start a GPT chat session.
With prefix argument USE-CONTEXT,
interactively select context to be used for this command."
  (interactive "P")
  (le-gpt-chat-start use-context))

;;;###autoload
(defun le-gpt-transform-region (&optional use-context)
  "Transform the selected region using GPT.
With prefix argument USE-CONTEXT,
interactively select context files to be used for this command."
  (interactive "P")
  (le-gpt-transform-region-with-prompt use-context))

;;;###autoload
(defun le-gpt-complete-at-point (&optional use-context)
  "Get completion suggestions from GPT at point.
With prefix argument USE-CONTEXT,
interactively select context files to be used for this command."
  (interactive "P")
  (le-gpt-completion-at-point use-context))

(defgroup le-gpt nil
  "Customization group for Le GPT."
  :group 'applications)

(provide 'le-gpt)

;;; le-gpt.el ends here
