;;; gpt-pilot-chat.el --- Chat functionality for gpt-pilot.el -*- lexical-binding: t; -*-
(require 'gpt-pilot-core)

(defcustom gpt-pilot-chat-use-named-buffers t
  "If non-nil, use named buffers for GPT output. Otherwise, use temporary buffers."
  :type 'boolean
  :group 'gpt-pilot)

;; Chat buffer creation and management
(defun gpt-pilot--chat-get-output-buffer-name (command)
  "Get the output buffer name for a given COMMAND."
  (let* ((truncated-command (substring command 0 (min gpt-pilot-chat-buffer-name-length (length command))))
         (ellipsis (if (< (length truncated-command) (length command)) "..." "")))
    (concat "*gpt"
            "[" (number-to-string gpt-pilot--chat-buffer-counter) "]: "
            truncated-command
            ellipsis
            "*")))

(defun gpt-pilot--chat-create-output-buffer (command)
  "Create a buffer to capture the output of the GPT process.
If `gpt-pilot-chat-use-named-buffers' is non-nil, create or get a named buffer.
Otherwise, create a temporary buffer. Use the `gpt-pilot-chat-mode' for the output buffer."
  (let ((output-buffer
         (if gpt-pilot-chat-use-named-buffers
             (let ((buffer (get-buffer-create (gpt-pilot--chat-get-output-buffer-name command))))
               (setq gpt-pilot--chat-buffer-counter (1+ gpt-pilot--chat-buffer-counter))  ; Increment the counter
               buffer)
           (generate-new-buffer (gpt-pilot--chat-get-output-buffer-name command)))))
    (with-current-buffer output-buffer
      (gpt-pilot-chat-mode))
    output-buffer))

(defun gpt-pilot--chat-insert-command (command)
  "Insert COMMAND to GPT in chat format into the current buffer."
  (let ((template "User: %s\n\nAssistant: "))
    (insert (format template command))))

(defun gpt-pilot--chat-run-buffer (buffer)
  "Run GPT command with BUFFER text as input and append output stream to output-buffer."
  (with-current-buffer buffer
    (goto-char (point-max))
    (font-lock-fontify-buffer)
    (gpt-pilot-make-process (gpt-pilot-create-prompt-file buffer) buffer)
    (message "GPT Pilot: Running command...")
    (font-lock-fontify-buffer)))

;; Chat commands (gpt-pilot-chat-start, gpt-pilot-chat-follow-up)
(defun gpt-pilot-chat-start (&optional all-buffers)
  "Run user-provided GPT command on region or all visible buffers and print output stream.
If called with a prefix argument (i.e., ALL-BUFFERS is non-nil), use all visible buffers as input.
Otherwise, use the current region."
  (let* ((initial-buffer (current-buffer))
         (command (gpt-pilot-read-command))
         (output-buffer (gpt-pilot--chat-create-output-buffer command))
         (input (if all-buffers
                    (gpt-pilot-get-visible-buffers-content)
                  (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))
         (project-context (gpt-pilot-get-project-context)))
    (switch-to-buffer-other-window output-buffer)
    (when project-context
      (insert (format "User:\n\n```\n%s\n```\n\n" project-context)))
    (when input
      (insert (format "User:\n\n```\n%s\n```\n\n" input)))
    (gpt-pilot--chat-insert-command command)
    (gpt-pilot--chat-run-buffer output-buffer)))

(defun gpt-pilot-chat-all-buffers ()
  "Run user-provided GPT command on all visible buffers and print output stream."
  (interactive)
  (gpt-pilot-chat t))

(defun gpt-pilot-chat-follow-up ()
  "Run a follow-up GPT command on the output buffer and append the output stream."
  (interactive)
  (unless (eq major-mode 'gpt-pilot-chat-mode)
    (user-error "Not in a gpt output buffer"))
  (let ((command (gpt-pilot-read-command)))
    (goto-char (point-max))
    (insert "\n\n")
    (gpt-pilot--chat-insert-command command)
    (gpt-pilot--chat-run-buffer (current-buffer))))

(defun gpt-pilot-chat-toggle-prefix ()
  "Toggle the visibility of the GPT prefixes."
  (interactive)
  (if (and (listp buffer-invisibility-spec)
           (memq 'gpt-pilot-chat-prefix buffer-invisibility-spec))
      (remove-from-invisibility-spec 'gpt-pilot-chat-prefix)
    (add-to-invisibility-spec 'gpt-pilot-chat-prefix))
  (font-lock-fontify-buffer))

(defun gpt-pilot-chat-copy-code-block ()
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

;; Buffer naming and management
(defcustom gpt-pilot-chat-generate-buffer-name-instruction "Create a title with a maximum of 50 chars for the chat above. Return a single title, nothing else. No quotes."
  "The instruction given to GPT to generate a buffer name."
  :type 'string
  :group 'gpt-pilot)

(defcustom gpt-pilot-chat-buffer-name-length 60
  "Maximum character length of the GPT buffer name title."
  :type 'integer
  :group 'gpt-pilot)

(defvar gpt-pilot--chat-buffer-counter 0
  "Counter to ensure unique buffer names for GPT output buffers.")

(defun gpt-pilot-chat-generate-buffer-name ()
  "Update the buffer name by asking GPT to create a title for it."
  (interactive)
  (unless (eq major-mode 'gpt-pilot-chat-mode)
    (user-error "Not in a gpt output buffer"))
  (let* ((gpt-pilot-buffer (current-buffer))
         (buffer-string (gpt-pilot--chat-buffer-string gpt-pilot-buffer))
         (prompt (concat buffer-string "\n\nUser: " gpt-pilot-chat-generate-buffer-name-instruction))
         (prompt-file (gpt-pilot-create-prompt-file prompt)))
    (with-temp-buffer
      (let ((process (gpt-pilot-make-process prompt-file (current-buffer))))
        (message "Asking GPT to generate buffer name...")
        (while (process-live-p process)
          (accept-process-output process))
        (let ((generated-title (string-trim (buffer-string))))
          (with-current-buffer gpt-pilot-buffer
            (rename-buffer (gpt-pilot--chat-get-output-buffer-name generated-title))))))))

(defun gpt-pilot--chat-buffer-string (buffer)
  "Get BUFFER text as string."
  (with-current-buffer buffer
    (buffer-string)))

;; Chat UI
;; Face definitions
(defface gpt-pilot-chat-input-face
  '((t :inherit comint-highlight-prompt))
  "Face for the input of the GPT commands.")

(defface gpt-pilot-chat-output-face
  '((t :inherit comint-highlight-prompt))
  "Face for the output of the GPT commands.")

;; Font-lock configuration
(defvar gpt-pilot-chat-font-lock-keywords
  '(("^\\(User:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible gpt-pilot-chat-prefix))
     (2 'gpt-pilot-chat-input-face))
    ("^\\(Assistant:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible gpt-pilot-chat-prefix))
     (2 'gpt-pilot-chat-output-face))
    ("```\\([\0-\377[:nonascii:]]*?\\)```"
     (1 'font-lock-constant-face))))

;; Dynamic mode creation
(defun gpt-pilot--chat-dynamically-define-gpt-pilot-chat-mode ()
  "Define `gpt-pilot-chat-mode` based on whether markdown-mode is available or not."
  (let ((parent-mode (if (fboundp 'markdown-mode)
                         'markdown-mode
                       'text-mode)))
    (eval
     `(define-derived-mode gpt-pilot-chat-mode ,parent-mode "GPT"
        "A mode for displaying the output of GPT commands."
        (message "GPT mode initialized with parent: %s" ',parent-mode)
        (setq-local word-wrap t)
        (setq-local font-lock-extra-managed-props '(invisible))
        (if (eq ',parent-mode 'markdown-mode)
            (progn
              (setq markdown-fontify-code-blocks-natively t)
              (setq font-lock-defaults
                    (list (append markdown-mode-font-lock-keywords gpt-pilot-chat-font-lock-keywords))))
          (progn
            (setq-local font-lock-defaults '(gpt-pilot-chat-font-lock-keywords))
            (font-lock-mode 1)
            (font-lock-fontify-buffer)))
        (add-to-invisibility-spec 'gpt-pilot-chat-prefix)))))

;; Initialize the mode
(gpt-pilot--chat-dynamically-define-gpt-pilot-chat-mode)

;; Add keybindings for chat-mode
(define-key gpt-pilot-chat-mode-map (kbd "C-c C-c") 'gpt-pilot-chat-follow-up)
(define-key gpt-pilot-chat-mode-map (kbd "C-c C-p") 'gpt-pilot-chat-toggle-prefix)
(define-key gpt-pilot-chat-mode-map (kbd "C-c C-b") 'gpt-pilot-chat-copy-code-block)
(define-key gpt-pilot-chat-mode-map (kbd "C-c C-t") 'gpt-pilot-chat-generate-buffer-name)

(provide 'gpt-pilot-chat)
;;; gpt-pilot-chat.el ends here
