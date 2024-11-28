;;; gpt-pilot.el --- Emacs on steroids with GPT -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Andreas Stuhlmueller, 2024- Anselm Coogan

;; Author: Andreas Stuhlmueller <andreas@ought.org>
;; Maintainer: Anselm Coogan <anselm.coogan@gmail.com>
;; Version: 1.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/AnselmC/gpt-pilot.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This package is a fork of https://github.com/stuhlmueller/gpt.el
;; It aims to provide more comprehensive GPT integration in Emacs than its parent.
;; Features include:
;; - Multiple chats with different models
;; - Completion at point
;; - Region transformations
;; - Project context awareness
;; - Chat history management

;;; Code:

(require 'gpt-pilot-core)
(require 'gpt-pilot-chat)
(require 'gpt-pilot-transform)
(require 'gpt-pilot-project)

;;;###autoload
(defun gpt-pilot-chat (&optional all-buffers)
  "Start a GPT chat session.
With prefix argument ALL-BUFFERS, include all visible buffers as context."
  (interactive "P")
  (gpt-pilot-chat-start all-buffers))

;;;###autoload
(defun gpt-pilot-transform-region ()
  "Transform the selected region using GPT."
  (interactive)
  (gpt-pilot-transform-region-with-prompt))

;;;###autoload
(defun gpt-pilot-complete-at-point ()
  "Get completion suggestions from GPT at point."
  (interactive)
  (gpt-pilot-completion-at-point))

(provide 'gpt-pilot)

;;; gpt-pilot.el ends here
