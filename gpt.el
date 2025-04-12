;;; gpt.el --- Run instruction-following language models -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 2.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This package defines a set of functions and variables for running
;; instruction-following language models like GPT-4 and Claude 3.5
;; Sonnet.  It allows the user to enter a command with history and
;; completion, and optionally use the current region as input.  The
;; output of the command is displayed in a temporary buffer with the
;; same major mode as the original buffer.  The output is streamed as
;; it is produced by the GPT process.  The user can enter a follow-up
;; command in the output buffer, which will provide the output, the
;; follow-up command to GPT as a new prompt.  The follow-up output
;; will be appended to the output buffer.  The user can view and
;; export the command history to a file.

;;; Code:

(require 'gpt-core)
(require 'gpt-api)
(require 'gpt-ui)
(require 'gpt-mode)

;; Initialize the mode
(gpt-dynamically-define-gpt-mode)

;; Make functions available to users
(provide 'gpt)

;;; gpt.el ends here
