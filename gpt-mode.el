;;; gpt-mode.el --- Mode-specific functionality for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 2.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; This file contains mode-specific functionality for gpt.el.

;;; Code:

(require 'gpt-core)

(declare-function gpt-chat-completion "gpt-ui" (&optional context-mode))
(declare-function gpt-insert-command "gpt-ui" (command))
(declare-function gpt-validate-api-key "gpt-core" nil)
(declare-function gpt-run-buffer "gpt-api" (output-buffer))
(declare-function gpt-message "gpt-api" (message))
(declare-function gpt-update-model-settings "gpt-core" nil)
(declare-function gpt-create-output-buffer "gpt-ui" (command))
(declare-function gpt-read-command "gpt-ui" (context-mode use-selection))
(declare-function gpt-create-prompt-file "gpt-api" (content))

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
  "Face for tool call blocks.
Used for [Searching for: ...] and [Got web search results]."
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
  "Scan for gpt thinking content up to LIMIT and set match data.
This function is intended for use in `font-lock-keywords'.
It searches for a block of text that starts after a \"^[Thinking...]\"
and ends before a \"^[Thinking done.]\", another \"^[Thinking...]\",
or the LIMIT.

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
   '(gpt--font-lock-scan-thinking-content . (0 'gpt-thinking-content-face))

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

(defun gpt--find-model-name (api-type model-id)
  "Find display name in `gpt-available-models' for API-TYPE and MODEL-ID."
  (cl-loop for (name . plist) in gpt-available-models
           when (and (eq (plist-get plist :api) api-type)
                     (equal (plist-get plist :id) model-id))
           return name))

(defun gpt-switch-model ()
  "Switch between OpenAI, Anthropic, and Google models."
  (interactive)
  (let* ((current-model-name (gpt--find-model-name gpt-api-type gpt-model))
         (prompt (format "Choose model (current: %s): "
                         (or current-model-name
                             (format "%s/%s" gpt-api-type gpt-model))))
         (choice (completing-read prompt
                                  (mapcar #'car gpt-available-models)
                                  nil t nil nil current-model-name))
         (model-info (cdr (assoc choice gpt-available-models))))
    (if model-info
        (progn
          (setq gpt-api-type (plist-get model-info :api)
                gpt-model (plist-get model-info :id))
          (gpt-update-model-settings)  ; Update max_tokens and thinking_budget
          (message "Switched to %s model: %s (max_tokens=%s, thinking_budget=%s)"
                   (symbol-name gpt-api-type) gpt-model
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
  (gpt-validate-api-key)
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
            (api-type-str (symbol-name gpt-api-type))
            (output-buffer (generate-new-buffer " *gpt-title-output*")))
        (gpt-message "Asking GPT to generate buffer name...")
        (unwind-protect
            (progn
              ;; Use call-process-region to pass API key via stdin (more secure)
              (with-temp-buffer
                (insert api-key "\n")
                (call-process-region (point-min) (point-max)
                                     gpt-python-path nil output-buffer nil
                                     gpt-script-path gpt-model gpt-max-tokens
                                     gpt-temperature api-type-str prompt-file))
              (let ((generated-title (string-trim (with-current-buffer output-buffer
                                                    (buffer-string)))))
                (with-current-buffer gpt-buffer
                  (rename-buffer (format "*GPT: %s*" generated-title)))))
          (when (buffer-live-p output-buffer)
            (kill-buffer output-buffer))
          (when (file-exists-p prompt-file)
            (delete-file prompt-file)))))))

(defun gpt-chat-clipboard ()
  "Run a GPT command using the current clipboard/kill-ring content as context.
Prompts for a command, creates a new GPT output buffer, inserts the
clipboard content as context, and runs GPT to generate a response.
Useful for quickly processing text copied from other applications."
  (interactive)
  (gpt-validate-api-key)
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
    (define-key map (kbd "C-c C-k") 'gpt-kill-process)
    ;; Thinking mode commands - using non-reserved sequences
    (define-key map (kbd "C-c C-j t") 'gpt-toggle-thinking)
    (define-key map (kbd "C-c C-j i") 'gpt-toggle-interleaved-thinking)
    (define-key map (kbd "C-c C-j w") 'gpt-toggle-web-search)
    (define-key map (kbd "C-c C-j s") 'gpt-thinking-status)
    (define-key map (kbd "C-c C-j m") 'gpt-chat-multi-models)
    map)
  "Keymap for GPT mode.")

(defun gpt--enable-word-wrap ()
  "Ensure GPT buffers wrap long lines by default."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(define-derived-mode gpt-mode text-mode "GPT"
  "A mode for displaying the output of GPT commands.
This mode provides syntax highlighting for GPT conversations and
integrates with markdown-mode if available."
  (setq-local font-lock-multiline t)
  (setq-local font-lock-extra-managed-props '(invisible))

  ;; Check if we should enhance with markdown features
  (if (and (fboundp 'markdown-mode)
           (bound-and-true-p gpt-use-markdown-mode))
      (gpt--setup-markdown-features)
    (gpt--setup-basic-features))

  (gpt--enable-word-wrap)
  (add-to-invisibility-spec 'gpt-prefix)
  ;; Use the keymap we defined earlier
  (use-local-map gpt-mode-map)
  ;; Ensure spinner slot present in mode-line (inactive by default)
  (gpt--ensure-spinner-in-mode-line))

(defun gpt-kill-process ()
  "Kill the running GPT process for the current GPT buffer, if any."
  (interactive)
  (unless (eq major-mode 'gpt-mode)
    (user-error "Not in a gpt output buffer"))
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (process-live-p proc))
        (progn
          (delete-process proc)
          (when (fboundp 'gpt--stop-spinner) (gpt--stop-spinner))
          (message "Killed GPT process"))
      (message "No running GPT process"))))

(declare-function markdown-mode "markdown-mode" nil)

(defvar markdown-fontify-code-blocks-natively)

(defun gpt--setup-markdown-features ()
  "Set up markdown-specific features for gpt-mode."
  ;; First apply markdown-mode settings
  (let ((_markdown-inhibit-mode-hooks t))  ; Prevent recursion, unused var
    (markdown-mode))
  ;; Then apply our customizations
  (setq major-mode 'gpt-mode)
  (setq mode-name "GPT")
  (setq markdown-fontify-code-blocks-natively t)
  ;; Combine markdown and GPT font-lock keywords
  (let ((markdown-keywords (if (and (boundp 'markdown-mode-font-lock-keywords)
                                    markdown-mode-font-lock-keywords)
                               markdown-mode-font-lock-keywords
                             ;; Fallback to current font-lock-defaults if available
                             (and font-lock-defaults
                                  (listp font-lock-defaults)
                                  (car font-lock-defaults)))))
    (setq font-lock-defaults
          (list (append (if (listp markdown-keywords)
                            markdown-keywords
                          nil)
                        gpt-font-lock-keywords)
                nil t nil nil)))
  ;; Re-apply our keymap
  (use-local-map gpt-mode-map))

(defun gpt--setup-basic-features ()
  "Set up basic `text-mode' features for gpt-mode."
  (setq-local font-lock-defaults '(gpt-font-lock-keywords nil t))
  (font-lock-mode 1)
  (font-lock-ensure))

;; --- Mode-line spinner ---
(defcustom gpt-mode-line-spinner-interval 0.1
  "Interval (seconds) between spinner frame updates."
  :type 'number
  :group 'gpt)

(defvar gpt--spinner-frames ["◐" "◓" "◑" "◒"]
  "Unicode frames used for the GPT mode-line spinner.")

(defvar-local gpt--spinner-timer nil)
(defvar-local gpt--spinner-index 0)
(defvar-local gpt--spinner-string "")
(defvar-local gpt--spinner-active nil)

(defun gpt--spinner-mode-line ()
  "Return the spinner string for the mode line."
  (if gpt--spinner-active gpt--spinner-string ""))

(defun gpt--ensure-spinner-in-mode-line ()
  "Ensure the GPT spinner appears in this buffer's mode line."
  (let ((elt '(:eval (gpt--spinner-mode-line))))
    (unless (member elt mode-line-format)
      (setq-local mode-line-format (append mode-line-format (list " " elt))))))

(defun gpt--start-spinner ()
  "Start the mode-line spinner in the current GPT buffer."
  (setq gpt--spinner-active t)
  (setq gpt--spinner-index 0)
  (setq gpt--spinner-string (aref gpt--spinner-frames 0))
  (force-mode-line-update t)
  (when (timerp gpt--spinner-timer)
    (cancel-timer gpt--spinner-timer)
    (setq gpt--spinner-timer nil))
  (setq gpt--spinner-timer
        (run-with-timer 0 gpt-mode-line-spinner-interval
                        (lambda ()
                          (when (and (buffer-live-p (current-buffer)) gpt--spinner-active)
                            (setq gpt--spinner-index (mod (1+ gpt--spinner-index)
                                                          (length gpt--spinner-frames)))
                            (setq gpt--spinner-string (aref gpt--spinner-frames gpt--spinner-index))
                            (force-mode-line-update t))))))

(defun gpt--stop-spinner ()
  "Stop the mode-line spinner in the current GPT buffer."
  (when (timerp gpt--spinner-timer)
    (cancel-timer gpt--spinner-timer)
    (setq gpt--spinner-timer nil))
  (setq gpt--spinner-active nil)
  (setq gpt--spinner-string "")
  (force-mode-line-update t))

(provide 'gpt-mode)
;;; gpt-mode.el ends here
