;;; le-gpt-chat.el --- Chat functionality for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Core chat functionality including buffer creation, command execution, and basic operations.

;;; Code:

(require 'le-gpt-core)
(require 'le-gpt-context)
(require 'markdown-mode)

(defcustom le-gpt-chat-generate-buffer-name-instruction 
  "Create a title with a maximum of 50 chars for the chat above. Return a single title, nothing else. No quotes."
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

;; Buffer management functions
(defun le-gpt--chat-get-output-buffer-name (raw-name)
  "Get the output buffer name for a given RAW-NAME."
  (let* ((truncated-name (substring raw-name 0 (min le-gpt-chat-buffer-name-length (length raw-name))))
         (ellipsis (if (< (length truncated-name) (length raw-name)) "..." "")))
    (concat "*le-gpt"
            "[" (number-to-string le-gpt--chat-buffer-counter) "]: "
            truncated-name
            ellipsis
            "*")))

(defun le-gpt--chat-create-buffer (raw-name)
  "Create a new chat buffer with RAW-NAME."
  (let ((output-buffer (get-buffer-create (le-gpt--chat-get-output-buffer-name raw-name))))
    (setq le-gpt--chat-buffer-counter (1+ le-gpt--chat-buffer-counter))
    output-buffer))

(defun le-gpt--chat-create-output-buffer (command)
  "Create a named buffer to capture the output of the GPT process for COMMAND."
  (let ((output-buffer (le-gpt--chat-create-buffer command)))
    (with-current-buffer output-buffer
      (le-gpt-chat-mode)
      (setq-local buffer-metadata `(:model ,le-gpt-model :timestamp ,(current-time-string))))
    output-buffer))

;; Chat operations
(defun le-gpt--chat-insert-command (command)
  "Insert COMMAND to GPT in chat format into the current buffer."
  (let ((template "User: %s\n\nAssistant: "))
    (insert (format template command))))

(defun le-gpt--chat-run-buffer (buffer)
  "Run GPT command in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (font-lock-update)
    (le-gpt--make-process (le-gpt--create-prompt-file buffer) buffer)
    (message "Le GPT: Running command...")
    (font-lock-update)))

(defun le-gpt--chat-buffer-string (buffer)
  "Get BUFFER text as string."
  (with-current-buffer buffer
    (buffer-string)))

(defun le-gpt--get-gpt-buffers ()
  "Return a list of GPT-related buffers."
  (sort
   (seq-filter (lambda (buf)
                 (string-match-p "\\*le-gpt\\[[0-9]+\\].*\\*" (buffer-name buf)))
               (buffer-list))
   (lambda (a b)
     (string< (buffer-name a) (buffer-name b)))))

;; Buffer name generation
(defun le-gpt-chat--generate-buffer-name (le-gpt-buffer)
  "Generate a buffer name for LE-GPT-BUFFER using GPT."
  (let* ((buffer-string (le-gpt--chat-buffer-string le-gpt-buffer))
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

;; Interactive commands
(defun le-gpt-chat-start (use-context)
  "Start chat with GPT in new buffer.
If USE-CONTEXT is non-nil, select context interactively."
  (let* ((context (if use-context (le-gpt--get-context) nil))
         (command (le-gpt--read-command))
         (output-buffer (le-gpt--chat-create-output-buffer command))
         (input (when (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end)))))
    (switch-to-buffer-other-window output-buffer)
    (when context
      (insert (format "User:\n\n%s\n\n" context)))
    (when input
      (insert (format "User:\n\n```\n%s\n```\n\n" input)))
    (le-gpt--chat-insert-command command)
    (le-gpt--chat-run-buffer output-buffer)))

(defun le-gpt-chat-follow-up ()
  "Run a follow-up GPT command on the output buffer."
  (interactive)
  (unless (derived-mode-p 'le-gpt-chat-mode)
    (user-error "Not in a gpt output buffer"))
  (let ((command (le-gpt--read-command)))
    (goto-char (point-max))
    (insert "\n\n")
    (le-gpt--chat-insert-command command)
    (le-gpt--chat-run-buffer (current-buffer))))

(defun le-gpt-chat-generate-buffer-name ()
  "Update the buffer name by asking GPT to create a title for it."
  (interactive)
  (unless (derived-mode-p 'le-gpt-chat-mode)
    (user-error "Not in a gpt output buffer"))
  (le-gpt-chat--generate-buffer-name (current-buffer)))

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

;; Chat mode definition
(defvar le-gpt-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "C-c C-c") 'le-gpt-chat-follow-up)
    (define-key map (kbd "C-c C-p") 'le-gpt-chat-toggle-prefix)
    (define-key map (kbd "C-c C-b") 'le-gpt-chat-copy-code-block)
    (define-key map (kbd "C-c C-t") 'le-gpt-chat-generate-buffer-name)
    (define-key map (kbd "C-c C-s") 'le-gpt-chat-save-buffer)
    map)
  "Keymap for `le-gpt-chat-mode'.")

(define-derived-mode le-gpt-chat-mode markdown-mode "Le GPT Chat"
  "Major mode for le-gpt-chat buffers derived from `markdown-mode'."
  :group 'le-gpt
  (setq-local word-wrap t)
  (setq-local font-lock-extra-managed-props '(invisible))
  (setq markdown-fontify-code-blocks-natively t)
  (setq font-lock-defaults
        (list (append markdown-mode-font-lock-keywords le-gpt--chat-font-lock-keywords)))
  (add-to-invisibility-spec 'le-gpt-chat-prefix))

(provide 'le-gpt-chat)
;;; le-gpt-chat.el ends here
