;;; gpt-ui.el --- UI functionality for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 2.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "24.4") (gpt-core "1.3") (gpt-api "1.3"))

;;; Commentary:

;; This file contains user interface functions and buffer management for gpt.el.

;;; Code:

(require 'gpt-core)
(require 'gpt-api)

(defvar gpt-buffer-counter 0
  "Counter to ensure unique buffer names for GPT output buffers.")

(defvar gpt-buffer-name-length 60
  "Maximum character length of the GPT buffer name title.")

(defun gpt-read-command (context-mode use-selection)
  "Read a GPT command from the user with history and completion.
Shows CONTEXT-MODE and selection status in the prompt.
USE-SELECTION determines whether selection will be included."
  (let* ((has-region (and use-selection (use-region-p)))
         (context-desc
          (cond
           ((eq context-mode 'all-buffers)
            (if has-region "all buffers + selection" "all buffers"))
           ((eq context-mode 'current-buffer)
            (if has-region "buffer + selection" "buffer"))
           ((and (null context-mode) has-region) "selection")
           (t nil)))
         (prompt (if context-desc
                     (format "%s [%s]: " gpt-model context-desc)
                   (format "%s: " gpt-model)))
         (cmd (gpt-read-command-with-space prompt
                                         gpt-command-history nil nil nil
                                         'gpt-command-history)))
    (if (string-equal cmd "n/a")
        ""
      (string-trim cmd))))

(defun gpt-get-buffer-content (buffer &optional include-metadata)
  "Get content from BUFFER.
Optionally include metadata if INCLUDE-METADATA is non-nil."
  (with-current-buffer buffer
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (if include-metadata
          (format "# %s (File %s)\n\n%s"
                  (buffer-name)
                  (or (buffer-file-name) "N/A")
                  content)
        content))))

(defun gpt-get-buffer-content-with-cursor (buffer &optional include-metadata)
  "Get content from BUFFER with a <cursor/> inserted at point.
If INCLUDE-METADATA is non-nil, prepend a header with buffer name and file path."
  (with-current-buffer buffer
    (let* ((before (buffer-substring-no-properties (point-min) (point)))
           (after (buffer-substring-no-properties (point) (point-max))))
      (if include-metadata
          (format "# %s (File %s)\n\n%s<cursor/>%s"
                  (buffer-name)
                  (or (buffer-file-name) "N/A")
                  before
                  after)
        (concat before "<cursor/>" after)))))

(defun gpt-get-context (context-mode)
  "Return text context based on CONTEXT-MODE.
Always insert <cursor/> in the current buffer.
Possible CONTEXT-MODE values:
- \='all-buffers: all visible buffers plus a <cursor/> in current one
- \='current-buffer: only the current buffer, with a <cursor/>
- nil: no buffer context
If there is an active region, append as \"Selected region:\"."
  (require 'subr-x)
  (let* ((has-region (use-region-p))
         (region-text (when has-region
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (buffer-content
          (cond
           ((eq context-mode 'all-buffers)
            (let ((current (current-buffer))
                  (visible-buffers (mapcar #'window-buffer (window-list))))
              (mapconcat
               (lambda (buf)
                 (if (eq buf current)
                     (gpt-get-buffer-content-with-cursor buf t)
                   (gpt-get-buffer-content buf t)))
               visible-buffers
               "\n\n")))
           ((eq context-mode 'current-buffer)
            (gpt-get-buffer-content-with-cursor (current-buffer) t))
           (t ""))))
    (cond
     ((and (not (string-empty-p buffer-content)) has-region)
      (format "Buffers (cursor is at <cursor/>):\n\n```\n%s\n```\n\nSelected region:\n\n```\n%s\n```"
              buffer-content region-text))
     ((not (string-empty-p buffer-content))
      (format "Buffers (cursor is at <cursor/>):\n\n```\n%s\n```" buffer-content))
     (has-region
      (format "Selected region:\n\n```\n%s\n```" region-text))
     (t ""))))

(defun gpt-get-output-buffer-name (command)
  "Get the output buffer name for a given COMMAND."
  (let* ((truncated-command (substring command 0 (min gpt-buffer-name-length (length command))))
         (ellipsis (if (< (length truncated-command) (length command)) "..." "")))
    (concat "*gpt"
            "[" (number-to-string gpt-buffer-counter) "]: "
            truncated-command
            ellipsis
            "*")))

(defun gpt-create-output-buffer (command)
  "Create a buffer to capture the output of the GPT process.
If `gpt-use-named-buffers' is non-nil, create or get a named buffer.
Otherwise, create a temporary buffer. Use the `gpt-mode' for the output buffer."
  (let ((output-buffer
         (if gpt-use-named-buffers
             (let ((buffer (get-buffer-create (gpt-get-output-buffer-name command))))
               (setq gpt-buffer-counter (1+ gpt-buffer-counter))  ; Increment the counter
               buffer)
           (generate-new-buffer (gpt-get-output-buffer-name command)))))
    (with-current-buffer output-buffer
      (gpt-mode))
    output-buffer))

(defun gpt-insert-command (command)
  "Insert COMMAND to GPT in chat format into the current buffer."
  (let ((template "User: %s\n\nAssistant: "))
    (insert (format template command))))

;;;###autoload
(defun gpt-chat (&optional context-mode)
  "Run user-provided GPT command with configurable context and print output stream.
CONTEXT-MODE can be:
- \='all-buffers: Use all visible buffers as context
- \='current-buffer: Use current buffer as context
- nil or \='none: Use no buffer context
In all cases, if there is an active region, it will be included."
  (interactive (list (let ((choice (completing-read "Context mode: "
                                                  '("all-buffers" "current-buffer" "none")
                                                  nil t)))
                      (unless (string= choice "none")
                        (intern choice)))))
  (let* ((command (gpt-read-command context-mode t))  ; Pass t to use selection
         (output-buffer (gpt-create-output-buffer command))
         (input (gpt-get-context context-mode)))
    ;; Validate command is not empty
    (when (string-empty-p (string-trim command))
      (user-error "Command cannot be empty"))
    ;; Add command to history
    (unless (string-empty-p command)
      (add-to-list 'gpt-command-history command))
    (switch-to-buffer-other-window output-buffer)
    (when (not (string-empty-p input))
      (insert (format "User:\n\n%s\n\n" input)))
    (gpt-insert-command command)
    (gpt-run-buffer output-buffer)))

;;;###autoload
(defun gpt-chat-all-buffers ()
  "Run GPT command with all visible buffers as context."
  (interactive)
  (gpt-chat 'all-buffers))

;;;###autoload
(defun gpt-chat-current-buffer ()
  "Run GPT command with current buffer as context."
  (interactive)
  (gpt-chat 'current-buffer))

;;;###autoload
(defun gpt-chat-no-context ()
  "Run GPT command with no buffer context."
  (interactive)
  (gpt-chat nil))

(defun gpt-follow-up ()
  "Run a follow-up GPT command on the output buffer and append the output stream."
  (interactive)
  (unless (eq major-mode 'gpt-mode)
    (user-error "Not in a gpt output buffer"))
  (let ((command (gpt-read-command nil nil)))
    (goto-char (point-max))
    (insert "\n\n")
    (gpt-insert-command command)
    (gpt-run-buffer (current-buffer))))

(defun gpt-chat-completion (&optional context-mode)
  "Complete text from cursor position using GPT with configurable context.
CONTEXT-MODE can be:
- \='all-buffers: Use all visible buffers as context
- \='current-buffer: Use current buffer as context
- nil or \='none: Use no buffer context"
  (interactive (list (let ((choice (completing-read "Context mode: "
                                                  '("all-buffers" "current-buffer" "none")
                                                  nil t)))
                      (unless (string= choice "none")
                        (intern choice)))))
  (let* ((full-context (gpt-get-context context-mode))
         (source-buffer-name (buffer-name))
         (output-buffer (gpt-create-output-buffer 
                        (format "Complete %s" source-buffer-name)))
         (instruction "Continue writing from the <cursor/> position. Match the style and format of the existing text. Provide as ``` code block with language indicator. No meta commentary."))
    ;; Switch to output buffer
    (switch-to-buffer-other-window output-buffer)
    ;; Insert context and instruction with tags
    (insert "User:\n\n"
            "<context>\n"
            full-context
            "\n</context>\n\n"
            "<instruction>\n"
            instruction
            "\n</instruction>\n\n")
    (gpt-insert-command instruction)
    ;; Run GPT
    (gpt-run-buffer output-buffer)))

;;;###autoload
(defun gpt-toggle-thinking ()
  "Toggle extended thinking mode for Anthropic models (enabled by default)."
  (interactive)
  (setq gpt-thinking-enabled (not gpt-thinking-enabled))
  (message "Extended thinking mode %s" 
           (if gpt-thinking-enabled "enabled" "disabled")))

;;;###autoload
(defun gpt-toggle-interleaved-thinking ()
  "Toggle interleaved thinking mode for Anthropic models (enabled by default)."
  (interactive)
  (setq gpt-interleaved-thinking (not gpt-interleaved-thinking))
  (message "Interleaved thinking mode %s" 
           (if gpt-interleaved-thinking "enabled" "disabled")))

;;;###autoload
(defun gpt-toggle-web-search ()
  "Toggle web search for Anthropic models."
  (interactive)
  (setq gpt-web-search (not gpt-web-search))
  (message "Web search %s" 
           (if gpt-web-search "enabled" "disabled")))

;;;###autoload
(defun gpt-thinking-status ()
  "Display current thinking mode settings."
  (interactive)
  (message "Thinking: %s | Interleaved: %s | Web Search: %s | Budget: %s tokens"
           (if gpt-thinking-enabled "ON" "OFF")
           (if gpt-interleaved-thinking "ON" "OFF")
           (if gpt-web-search "ON" "OFF")
           gpt-thinking-budget))

(provide 'gpt-ui)
;;; gpt-ui.el ends here 