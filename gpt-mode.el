;;; gpt-mode.el --- Mode-specific functionality for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 1.4
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "24.4") (gpt-core "1.3"))

;;; Commentary:

;; This file contains mode-specific functionality for gpt.el.

;;; Code:

(require 'gpt-core)

(defface gpt-input-face
  '((t :inherit comint-highlight-prompt))
  "Face for the input of the GPT commands.")

(defface gpt-output-face
  '((t :inherit default))
  "Face for the output of the GPT commands.")

(defvar gpt-font-lock-keywords
  '(("^\\(User:\\|Human:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible gpt-prefix))
     (2 'gpt-input-face))
    ("^\\(Assistant:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible gpt-prefix))
     (2 'gpt-output-face))
    ("```\\([\0-\377[:nonascii:]]*?\\)```"  ; match code snippets enclosed in backticks
     (1 'font-lock-constant-face))))

(defun gpt-toggle-prefix ()
  "Toggle the visibility of the GPT prefixes."
  (interactive)
  (if (and (listp buffer-invisibility-spec)
           (memq 'gpt-prefix buffer-invisibility-spec))
      (remove-from-invisibility-spec 'gpt-prefix)
    (add-to-invisibility-spec 'gpt-prefix))
  (font-lock-fontify-buffer))

(defun gpt-copy-code-block ()
  "Copy the content of the code block at point to the clipboard."
  (interactive)
  (let* ((start (if (search-backward "\n```" nil t) (point) nil))
         (_ (goto-char (or (+ start 3) (point-min))))
         (end (if (search-forward "\n```" nil t) (point) nil)))
    (when (and start end)
      (let* ((content (buffer-substring-no-properties (+ start 3) (- end 3)))
             (lang-end (string-match "\n" content))
             (code (if lang-end
                       (substring content (+ lang-end 1))
                     content)))
        (kill-new code)
        (message "Code block copied to clipboard.")))))

(defun gpt-switch-model ()
  "Switch between OpenAI, Anthropic, and Google models."
  (interactive)
  (let* ((models '(("GPT-4o" . (openai . "gpt-4o"))
                   ("GPT-4.5" . (openai . "gpt-4.5-preview"))
                   ("o1" . (openai . "o1"))
                   ("o3-mini" . (openai . "o3-mini"))
                   ("Claude 3.7 Sonnet" . (anthropic . "claude-3-7-sonnet-latest"))
                   ;; Add Google models
                   ("Gemini 2.5 Pro Exp" . (google . "gemini-2.5-pro-exp-03-25"))))
         (choice (completing-read "Choose model: " (mapcar #'car models) nil t nil nil (car (rassoc (cons gpt-api-type gpt-model) models))))
         (model-info (cdr (assoc choice models))))
    (if model-info
        (progn
          (setq gpt-api-type (car model-info)
                gpt-model (cdr model-info))
          (message "Switched to %s model: %s" (symbol-name gpt-api-type) (cdr model-info)))
      (message "Model selection cancelled."))))

(defun gpt-close-current ()
  "Close the current GPT output buffer."
  (interactive)
  (when (eq major-mode 'gpt-mode)
    (kill-buffer (current-buffer))
    (message "Closed GPT buffer")))

(defun gpt-close-all ()
  "Close all GPT output buffers and their windows."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (eq major-mode 'gpt-mode))
      (kill-buffer buffer)))
  (message "Closed all GPT buffers"))

(defun gpt-generate-buffer-name ()
  "Update the buffer name by asking GPT to create a title for it."
  (interactive)
  (unless (eq major-mode 'gpt-mode)
    (user-error "Not in a gpt output buffer"))
  (let* ((gpt-buffer (current-buffer))
         (buffer-string (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (concat buffer-string "\n\nUser: Create a title with a maximum of 50 chars for the chat above. Say only the title, nothing else. No quotes.")))
    (with-temp-buffer
      (insert prompt)
      (let ((prompt-file (gpt-create-prompt-file (current-buffer)))
            (api-key (if (eq gpt-api-type 'openai) gpt-openai-key gpt-anthropic-key))
            (api-type-str (symbol-name gpt-api-type)))
        (erase-buffer)
        (gpt-message "Asking GPT to generate buffer name...")
        (call-process gpt-python-path nil t nil
                      gpt-script-path api-key gpt-model gpt-max-tokens gpt-temperature api-type-str prompt-file)
        (let ((generated-title (string-trim (buffer-string))))
          (with-current-buffer gpt-buffer
            (rename-buffer (format "*GPT: %s*" generated-title))))))))

(defun gpt-chat-clipboard ()
  "Run GPT command with clipboard content as context."
  (interactive)
  (let* ((clipboard-text (current-kill 0))
         (command (gpt-read-command nil t))
         (output-buffer (gpt-create-output-buffer command)))
    (switch-to-buffer-other-window output-buffer)
    (insert "User:\n\nClipboard content:\n\n```\n" clipboard-text "\n```\n\n")
    (gpt-insert-command command)
    (gpt-run-buffer output-buffer)))

(defun gpt-chat-completion-current-buffer ()
  "Complete text from cursor position using current buffer as context."
  (interactive)
  (gpt-chat-completion 'current-buffer))

(defun gpt-chat-completion-all-buffers ()
  "Complete text from cursor position using all visible buffers as context."
  (interactive)
  (gpt-chat-completion 'all-buffers))

(defvar gpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'gpt-follow-up)
    (define-key map (kbd "C-c C-p") 'gpt-toggle-prefix)
    (define-key map (kbd "C-c C-b") 'gpt-copy-code-block)
    (define-key map (kbd "C-c C-m") 'gpt-switch-model)
    (define-key map (kbd "C-c C-t") 'gpt-generate-buffer-name)
    (define-key map (kbd "C-c C-q") 'gpt-close-current)
    (define-key map (kbd "C-c C-x") 'gpt-close-all)
    map)
  "Keymap for GPT mode.")

(defun gpt-dynamically-define-gpt-mode ()
  "Define `gpt-mode` based on whether markdown-mode is available or not."
  (let ((parent-mode (if (fboundp 'markdown-mode)
                         'markdown-mode
                       'text-mode)))
    (eval
     ;; the `define-derived-mode` macro expects a literal as its first argument
     ;; hence, we can not simply use the `parent-mode` variable
     ;; but need to use a backquoted list and eval it
     `(define-derived-mode gpt-mode ,parent-mode "GPT"
        "A mode for displaying the output of GPT commands."
        (message "GPT mode intialized with parent: %s" ',parent-mode)
        (setq-local word-wrap t)
        (setq-local font-lock-extra-managed-props '(invisible))
        (if (eq ',parent-mode 'markdown-mode)
            (progn
              (setq markdown-fontify-code-blocks-natively t)
              ;; Combine markdown-mode's keywords with custom keywords
              (setq font-lock-defaults
                    (list (append markdown-mode-font-lock-keywords gpt-font-lock-keywords))))
          (progn
            (setq-local font-lock-defaults '(gpt-font-lock-keywords))
            (font-lock-mode 1)
            (font-lock-fontify-buffer))
          )
        (add-to-invisibility-spec 'gpt-prefix)))))

(provide 'gpt-mode)
;;; gpt-mode.el ends here 