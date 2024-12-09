;;; le-gpt-chat.el --- Chat functionality for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;

;;; Code:

(require 'le-gpt-core)
(require 'le-gpt-project)
(require 'markdown-mode)

(defcustom le-gpt-chat-use-named-buffers t
  "If non-nil, use named buffers for GPT output.  Otherwise, use temporary buffers."
  :type 'boolean
  :group 'le-gpt)

(defcustom le-gpt-chat-generate-buffer-name-instruction "Create a title with a maximum of 50 chars for the chat above. Return a single title, nothing else. No quotes."
  "The instruction given to GPT to generate a buffer name."
  :type 'string
  :group 'le-gpt)

(defcustom le-gpt-chat-buffer-name-length 60
  "Maximum character length of the GPT buffer name title."
  :type 'integer
  :group 'le-gpt)

(defvar le-gpt--chat-buffer-counter 0
  "Counter to ensure unique buffer names for GPT output buffers.")

(defface le-gpt-chat-input-face
  '((t :inherit comint-highlight-prompt))
  "Face for the input of the GPT commands.")

(defface le-gpt-chat-output-face
  '((t :inherit comint-highlight-prompt))
  "Face for the output of the GPT commands.")

(defvar le-gpt--chat-font-lock-keywords
  '(("^\\(User:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible le-gpt-chat-prefix))
     (2 'le-gpt-chat-input-face))
    ("^\\(Assistant:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible le-gpt-chat-prefix))
     (2 'le-gpt-chat-output-face))
    ("```\\([\0-\377[:nonascii:]]*?\\)```"
     (1 'font-lock-constant-face))))

(defun le-gpt--chat-get-output-buffer-name (command)
  "Get the output buffer name for a given COMMAND."
  (let* ((truncated-command (substring command 0 (min le-gpt-chat-buffer-name-length (length command))))
         (ellipsis (if (< (length truncated-command) (length command)) "..." "")))
    (concat "*le-gpt"
            "[" (number-to-string le-gpt--chat-buffer-counter) "]: "
            truncated-command
            ellipsis
            "*")))

(defun le-gpt--chat-create-output-buffer (command)
  "Create a buffer to capture the output of the GPT process for COMMAND.
If `le-gpt-chat-use-named-buffers' is non-nil, create or get a named buffer.
Otherwise, create a temporary buffer.
Use the `le-gpt-chat-mode' for the output buffer."
  (let ((output-buffer
         (if le-gpt-chat-use-named-buffers
             (let ((buffer (get-buffer-create (le-gpt--chat-get-output-buffer-name command))))
               (setq le-gpt--chat-buffer-counter (1+ le-gpt--chat-buffer-counter))  ; Increment the counter
               buffer)
           (generate-new-buffer (le-gpt--chat-get-output-buffer-name command)))))
    (with-current-buffer output-buffer
      (le-gpt-chat-mode)
      (setq-local buffer-metadata `(:model ,le-gpt-model :timestamp ,(current-time-string))))
    output-buffer))

(defun le-gpt--chat-insert-command (command)
  "Insert COMMAND to GPT in chat format into the current buffer."
  (let ((template "User: %s\n\nAssistant: "))
    (insert (format template command))))

(defun le-gpt--chat-run-buffer (buffer)
  "Run GPT command in BUFFER.
Provide text in buffer as input & append stream to BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (font-lock-update)
    (le-gpt--make-process (le-gpt--create-prompt-file buffer) buffer)
    (message "Le GPT: Running command...")
    (font-lock-update)))

(defun le-gpt-chat-start (temp-context-files)
  "Start chat with GPT in new buffer.
If region is active, use the region as input.
Otherwise, use the entire buffer as input.
If TEMP-CONTEXT-FILES is non-nil, select context files interactively."
  (let* ((project-context (le-gpt--get-project-context temp-context-files))
         (command (le-gpt--read-command))
         (output-buffer (le-gpt--chat-create-output-buffer command))
         (input (when (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end)))))
    (switch-to-buffer-other-window output-buffer)
    (when project-context
      (insert (format "User:\n\n```\n%s\n```\n\n" project-context)))
    (when input
      (insert (format "User:\n\n```\n%s\n```\n\n" input)))
    (le-gpt--chat-insert-command command)
    (le-gpt--chat-run-buffer output-buffer)))

(defun le-gpt-chat-follow-up ()
  "Run a follow-up GPT command on the output buffer and append the output stream."
  (interactive)
  (unless (derived-mode-p 'le-gpt-chat-mode)
    (user-error "Not in a gpt output buffer"))
  (let ((command (le-gpt--read-command)))
    (goto-char (point-max))
    (insert "\n\n")
    (le-gpt--chat-insert-command command)
    (le-gpt--chat-run-buffer (current-buffer))))

(defun le-gpt-chat-toggle-prefix ()
  "Toggle the visibility of the GPT prefixes."
  (interactive)
  (if (and (listp buffer-invisibility-spec)
           (memq 'le-gpt-chat-prefix buffer-invisibility-spec))
      (remove-from-invisibility-spec 'le-gpt-chat-prefix)
    (add-to-invisibility-spec 'le-gpt-chat-prefix))
  (font-lock-update))

(defun le-gpt-chat-copy-code-block ()
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

(defun le-gpt-chat-generate-buffer-name ()
  "Update the buffer name by asking GPT to create a title for it."
  (interactive)
  (unless (derived-mode-p 'le-gpt-chat-mode)
    (user-error "Not in a gpt output buffer"))
  (let* ((le-gpt-buffer (current-buffer))
         (buffer-string (le-gpt--chat-buffer-string le-gpt-buffer))
         (prompt (concat buffer-string "\n\nUser: " le-gpt-chat-generate-buffer-name-instruction))
         (prompt-file (le-gpt--create-prompt-file prompt)))
    (with-temp-buffer
      (let ((process (le-gpt--make-process prompt-file (current-buffer))))
        (message "Asking GPT to generate buffer name...")
        (while (process-live-p process)
          (accept-process-output process))
        (let ((generated-title (string-trim (buffer-string))))
          (with-current-buffer le-gpt-buffer
            (rename-buffer (le-gpt--chat-get-output-buffer-name generated-title))))))))

(defun le-gpt--chat-buffer-string (buffer)
  "Get BUFFER text as string."
  (with-current-buffer buffer
    (buffer-string)))


(define-derived-mode le-gpt-chat-mode markdown-mode "Le GPT Chat"
  "Major mode for le-gpt-chat buffers derived from `markdown-mode'."
  :group 'le-gpt
  (setq-local word-wrap t)
  (setq-local font-lock-extra-managed-props '(invisible))
  (setq markdown-fontify-code-blocks-natively t)
  (setq font-lock-defaults
        (list (append markdown-mode-font-lock-keywords le-gpt--chat-font-lock-keywords)))
  (add-to-invisibility-spec 'le-gpt-chat-prefix))

(defvar le-gpt-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'le-gpt-chat-follow-up)
    (define-key map (kbd "C-c C-p") 'le-gpt-chat-toggle-prefix)
    (define-key map (kbd "C-c C-b") 'le-gpt-chat-copy-code-block)
    (define-key map (kbd "C-c C-t") 'le-gpt-chat-generate-buffer-name)
    map)
  "Keymap for `le-gpt-chat-mode'.")

(define-derived-mode le-gpt-buffer-list-mode tabulated-list-mode "Le-GPT Buffers"
  "Major mode for listing GPT buffers.
\\{le-gpt-buffer-list-mode-map}"
  (setq truncate-lines t)
  (setq tabulated-list-format [("M" 1 t)
                               ("Buffer name" 80 t)
                               ("Model" 20 t)
                               ("Created at" 20 t)])
  (tabulated-list-init-header))

(defvar le-gpt-buffer-list-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keep the original bindings
    (define-key map (kbd "RET") #'le-gpt-buffer-list-open-buffer)
    (define-key map (kbd "d") #'le-gpt-buffer-list-mark-delete)
    (define-key map (kbd "u") #'le-gpt-buffer-list-unmark)
    (define-key map (kbd "x") #'le-gpt-buffer-list-execute)
    (define-key map (kbd "g r") #'le-gpt-buffer-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `le-gpt-buffer-list-mode'.")


(defun le-gpt-list-buffers ()
  "Display a list of all GPT buffers."
  (interactive)
  (let ((buf (get-buffer "*Le GPT Buffers*")))
    (unless buf
      (setq buf (get-buffer-create "*Le GPT Buffers*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (le-gpt-buffer-list-mode)
        (le-gpt-buffer-list-refresh)
        (goto-char (point-min)))
      (display-buffer buf '(display-buffer-same-window)))))

(defun le-gpt-buffer-list-refresh ()
  "Refresh the GPT buffers list."
  (interactive)
  (let ((inhibit-read-only t)
        (gpt-buffers (le-gpt--get-gpt-buffers)))
    (erase-buffer)
    (setq tabulated-list-entries
          (mapcar (lambda (buffer)
                    (let ((name (buffer-name buffer))
                          (model (with-current-buffer buffer
                                   (if (local-variable-p 'buffer-metadata)
                                       (plist-get buffer-metadata :model)
                                     "n/a")))
                          (timestamp (with-current-buffer buffer
                                       (if (local-variable-p 'buffer-metadata)
                                           (or (plist-get buffer-metadata :timestamp) "n/a")
                                         "n/a"))))
                      (list buffer
                            (vector " " name model timestamp))))
                  gpt-buffers))
    (tabulated-list-print t)))

(defun le-gpt--get-gpt-buffers ()
  "Return a list of GPT-related buffers."
  (sort
   (seq-filter (lambda (buf)
                 (string-match-p "\\*le-gpt\\[[0-9]+\\].*\\*" (buffer-name buf)))
               (buffer-list))
   (lambda (a b)
     (string< (buffer-name a) (buffer-name b)))))

(defun le-gpt-buffer-list-open-buffer ()
  "Open the GPT buffer at point."
  (interactive)
  (let ((buffer-name (le-gpt-buffer-list-get-buffer-name)))
    (message "OPENING %s" buffer-name)
    (when buffer-name
      (select-window (display-buffer (get-buffer buffer-name)
                                     '(display-buffer-same-window))))))

(defun le-gpt-buffer-list-get-buffer-name ()
  "Get the buffer name from the current line."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^.\\{2\\}\\(\\*le-gpt\\[[0-9]+\\].*\\*\\)" (line-end-position) t)
      (match-string 1))))

(defun le-gpt-buffer-list-mark-delete ()
  "Mark a GPT buffer for deletion."
  (interactive)
  (when (tabulated-list-get-id)
    (let ((inhibit-read-only t))
      (tabulated-list-set-col 0 "D" t)
      (forward-line 1))))

(defun le-gpt-buffer-list-unmark ()
  "Unmark a GPT buffer."
  (interactive)
  (when (tabulated-list-get-id)
    (let ((inhibit-read-only t))
      (tabulated-list-set-col 0 " " t)
      (forward-line 1))))

(defun le-gpt-buffer-list-execute ()
  "Execute marked operations in the GPT buffer list."
  (interactive)
  (let ((marked-buffers '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((entry (tabulated-list-get-entry))
                    (mark (aref entry 0))
                    ((string= mark "D"))
                    (buffer (tabulated-list-get-id)))
          (push buffer marked-buffers))
        (forward-line 1)))
    (dolist (buffer marked-buffers)
      (kill-buffer buffer))
    (le-gpt-buffer-list-refresh)))

(provide 'le-gpt-chat)
;;; le-gpt-chat.el ends here
