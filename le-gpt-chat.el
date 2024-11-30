;;; le-gpt-chat.el --- Chat functionality for le-gpt.el -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'le-gpt-core)
(require 'le-gpt-project)

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
    (concat "*gpt"
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
      (le-gpt-chat-mode))
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
    (message "GPT Pilot: Running command...")
    (font-lock-update)))

(defun le-gpt--chat-get-visible-buffers-content ()
  "Get content, buffer name, and file name (optional) of all visible buffers."
  (let ((visible-buffers (mapcar 'window-buffer (window-list)))
        contents)
    (dolist (buffer visible-buffers contents)
      (with-current-buffer buffer
        (push (format "Buffer Name: %s\nFile Name: %s\nContent:\n%s"
                      (buffer-name)
                      (or (buffer-file-name) "N/A")
                      (buffer-substring-no-properties (point-min) (point-max)))
              contents)))
    (mapconcat 'identity (nreverse contents) "\n\n")))

;; Chat commands (le-gpt-chat-start, le-gpt-chat-follow-up)
(defun le-gpt-chat-start (&optional all-buffers)
  "Start chat with GPT in new buffer.
If called with a prefix argument (i.e., ALL-BUFFERS is non-nil),
use all visible buffers as input.
Otherwise, use the current region."
  (let* ((command (le-gpt--read-command))
         (output-buffer (le-gpt--chat-create-output-buffer command))
         (input (if all-buffers
                    (le-gpt--chat-get-visible-buffers-content)
                  (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))))
         (project-context (le-gpt-get-project-context)))
    (switch-to-buffer-other-window output-buffer)
    (when project-context
      (insert (format "User:\n\n```\n%s\n```\n\n" project-context)))
    (when input
      (insert (format "User:\n\n```\n%s\n```\n\n" input)))
    (le-gpt--chat-insert-command command)
    (le-gpt--chat-run-buffer output-buffer)))

(defun le-gpt-chat-all-buffers ()
  "Run user-provided GPT command on all visible buffers and print output stream."
  (interactive)
  (le-gpt-chat-start t))

(defun le-gpt-chat-follow-up ()
  "Run a follow-up GPT command on the output buffer and append the output stream."
  (interactive)
  (unless (eq major-mode 'le-gpt-chat-mode)
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

;; Buffer naming and management

(defun le-gpt-chat-generate-buffer-name ()
  "Update the buffer name by asking GPT to create a title for it."
  (interactive)
  (unless (eq major-mode 'le-gpt-chat-mode)
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


;; Dynamic mode creation
(defun le-gpt--chat-dynamically-define-le-gpt-chat-mode ()
  "Define `le-gpt-chat-mode' based on whether `markdown-mode' is available or not."
  (let ((parent-mode (if (fboundp 'markdown-mode)
                         'markdown-mode
                       'text-mode)))
    (eval
     `(define-derived-mode le-gpt-chat-mode ,parent-mode "GPT"
        "A mode for displaying the output of GPT commands."
        (message "GPT mode initialized with parent: %s" ',parent-mode)
        (setq-local word-wrap t)
        (setq-local font-lock-extra-managed-props '(invisible))
        (if (eq ',parent-mode 'markdown-mode)
            (progn
              (setq markdown-fontify-code-blocks-natively t)
              (setq font-lock-defaults
                    (list (append markdown-mode-font-lock-keywords le-gpt--chat-font-lock-keywords))))
          (progn
            (setq-local font-lock-defaults '(le-gpt--chat-font-lock-keywords))
            (font-lock-mode 1)
            (font-lock-update)))
        (add-to-invisibility-spec 'le-gpt-chat-prefix)))))

;; Initialize the mode
(le-gpt--chat-dynamically-define-le-gpt-chat-mode)

;; Add keybindings for chat-mode
(define-key le-gpt-chat-mode-map (kbd "C-c C-c") 'le-gpt-chat-follow-up)
(define-key le-gpt-chat-mode-map (kbd "C-c C-p") 'le-gpt-chat-toggle-prefix)
(define-key le-gpt-chat-mode-map (kbd "C-c C-b") 'le-gpt-chat-copy-code-block)
(define-key le-gpt-chat-mode-map (kbd "C-c C-t") 'le-gpt-chat-generate-buffer-name)

(provide 'le-gpt-chat)
;;; le-gpt-chat.el ends here
