;;; gpt-mode.el --- Mode-specific functionality for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 2.0
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
  '((t (:inherit comint-highlight-prompt)))
  "Face for the input of the GPT commands."
  :group 'gpt)

(defface gpt-output-face
  '((t (:inherit default)))
  "Face for the output of the GPT commands."
  :group 'gpt)

(defface gpt-thinking-delimiter-face
  '((((class color) (min-colors 88) (background light)) :foreground "darkgreen" :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "springgreen" :weight bold)
    (t :inherit font-lock-keyword-face :weight bold))
  "Face for [Thinking...] and [Thinking done.] delimiters."
  :group 'gpt)

(defface gpt-thinking-content-face
  '((((class color) (min-colors 88) (background light)) :foreground "gray50")
    (((class color) (min-colors 88) (background dark)) :foreground "gray70")
    (t :inherit font-lock-comment-face))
  "Face for content within a thinking block."
  :group 'gpt)

(defface gpt-tool-call-face
  '((((class color) (min-colors 88) (background light)) :foreground "royalblue" :weight bold)
    (((class color) (min-colors 88) (background dark)) :foreground "skyblue" :weight bold)
    (t :inherit font-lock-function-name-face :weight bold))
  "Face for tool call blocks like [Searching for: ...] and [Got web search results]."
  :group 'gpt)

(defvar gpt-font-lock-keywords
  '(("^\\(User:\\|Human:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible gpt-prefix))
     (2 'gpt-input-face))
    ("^\\(Assistant:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible gpt-prefix))
     (2 'gpt-output-face))
    ("```\\([\0-\377[:nonascii:]]*?\\)```"  ; match code snippets enclosed in backticks
     (1 'font-lock-constant-face))))

(defvar gpt--old-font-lock-keywords gpt-font-lock-keywords
  "Original gpt-font-lock-keywords for User/Assistant and code blocks.")

(defun gpt--font-lock-scan-thinking-content (limit)
  "Scan for gpt thinking content and set match data.
This function is intended for use in `font-lock-keywords'.
It searches for a block of text that starts after a \"^[Thinking...]\"
and ends before a \"^[Thinking done.]\", another \"^[Thinking...]\",
or the `limit`.

Point is moved to the end of the matched content region if a match is found.
Returns t if a match is found and sets match-data, nil otherwise."
  (let (found-match)
    ;; Search for the start of a thinking block from the current point.
    (when (re-search-forward "^\\[Thinking\\.\\.\\.\\]" limit t)
      (let ((block-content-start (match-end 0)) ; Content starts after the delimiter
            (block-content-end limit))          ; Default end is the search limit

        ;; From the start of the content, search for the end delimiter
        ;; or the start of a new thinking block.
        (save-excursion
          (goto-char block-content-start)
          (if (re-search-forward "^\\(\\[Thinking done\\.\\]\\|\\[Thinking\\.\\.\\.\\]\\)" limit t)
              ;; If found, content ends before this new delimiter
              (setq block-content-end (match-beginning 0))))

        ;; Check if there is actual content to highlight
        (when (> block-content-end block-content-start)
          (set-match-data (list block-content-start block-content-end))
          ;; Move point to the end of the matched content for font-lock
          (goto-char block-content-end)
          (setq found-match t))))
    found-match))

(defvar gpt-highlighting-keywords
  (list
   ;; Rule for the content within thinking blocks.
   ;; This is processed first.
   '(gpt--font-lock-scan-thinking-content . 'gpt-thinking-content-face)

   ;; Rules for delimiters. These have 'override' set to true (the `t` at the end),
   ;; so they will apply their face even if the line was already faced by the content rule.
   '("^\\[Thinking\\.\\.\\.\\]"       (0 'gpt-thinking-delimiter-face t))
   '("^\\[Thinking done\\.\\]"     (0 'gpt-thinking-delimiter-face t))

   ;; Rules for tool call lines. Also with override.
   '("^\\[Searching for: [^\n]*\\]" (0 'gpt-tool-call-face t))
   '("^\\[Got web search results\\]" (0 'gpt-tool-call-face t)))
  "Font-lock keywords for gpt-style output buffers.
The order is important: content is matched by the function,
then specific delimiter lines override the content face.")

;; Combine new keywords with old ones
(setq gpt-font-lock-keywords (append gpt-highlighting-keywords gpt--old-font-lock-keywords))

(defun gpt-toggle-prefix ()
  "Toggle the visibility of the GPT prefixes."
  (interactive)
  (if (and (listp buffer-invisibility-spec)
           (memq 'gpt-prefix buffer-invisibility-spec))
      (remove-from-invisibility-spec 'gpt-prefix)
    (add-to-invisibility-spec 'gpt-prefix))
  (font-lock-flush))

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
  (let* ((current-model-name (car (rassoc (cons gpt-api-type gpt-model) gpt-available-models)))
         (prompt (format "Choose model (current: %s): " 
                         (or current-model-name 
                             (format "%s/%s" gpt-api-type gpt-model))))
         (choice (completing-read prompt
                                  (mapcar #'car gpt-available-models) 
                                  nil t nil nil current-model-name))
         (model-info (cdr (assoc choice gpt-available-models))))
    (if model-info
        (progn
          (setq gpt-api-type (car model-info)
                gpt-model (cdr model-info))
          (gpt-update-model-settings)  ; Update max_tokens and thinking_budget
          (message "Switched to %s model: %s (max_tokens=%s, thinking_budget=%s)" 
                   (symbol-name gpt-api-type) (cdr model-info)
                   gpt-max-tokens gpt-thinking-budget))
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
            (api-key (cond ((eq gpt-api-type 'openai) gpt-openai-key)
                           ((eq gpt-api-type 'anthropic) gpt-anthropic-key)
                           ((eq gpt-api-type 'google) gpt-google-key)
                           (t "NOT SET")))
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

(defun gpt-regenerate-response ()
  "Regenerate the last assistant response."
  (interactive)
  (let ((start (save-excursion
                 (if (re-search-backward "^Assistant:" nil t)
                     (point)
                   (point-min))))
        (end (point-max)))
    (when (> end start)
      (delete-region start end)
      (gpt-run-buffer (current-buffer)))))

(defvar gpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'gpt-follow-up)
    (define-key map (kbd "C-c C-p") 'gpt-toggle-prefix)
    (define-key map (kbd "C-c C-b") 'gpt-copy-code-block)
    (define-key map (kbd "C-c C-m") 'gpt-switch-model)
    (define-key map (kbd "C-c C-t") 'gpt-generate-buffer-name)
    (define-key map (kbd "C-c C-q") 'gpt-close-current)
    (define-key map (kbd "C-c C-x") 'gpt-close-all)
    (define-key map (kbd "C-c C-r") 'gpt-regenerate-response)
    ;; Thinking mode commands
    (define-key map (kbd "C-c T t") 'gpt-toggle-thinking)
    (define-key map (kbd "C-c T i") 'gpt-toggle-interleaved-thinking)
    (define-key map (kbd "C-c T w") 'gpt-toggle-web-search)
    (define-key map (kbd "C-c T s") 'gpt-thinking-status)
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
        (message "GPT mode initialized with parent: %s" ',parent-mode)
        (setq-local word-wrap t)
        (setq-local font-lock-multiline t) ; Ensure `.` can match newline for our regexps
        (setq-local font-lock-extra-managed-props '(invisible))
        (if (eq ',parent-mode 'markdown-mode)
            (progn
              (setq markdown-fontify-code-blocks-natively t)
              ;; Combine markdown-mode's keywords with custom keywords
              (setq font-lock-defaults
                    (list (append markdown-mode-font-lock-keywords gpt-font-lock-keywords))))
          (progn
            (setq-local font-lock-defaults '(gpt-font-lock-keywords nil t)) ; Added nil t for no-keyword-छेदन and multiline
            (font-lock-mode 1)
            (font-lock-ensure))
          )
        (add-to-invisibility-spec 'gpt-prefix)))))

(provide 'gpt-mode)
;;; gpt-mode.el ends here 
