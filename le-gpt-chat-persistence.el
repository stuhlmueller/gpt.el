;;; le-gpt-chat-persistence.el --- Save and load functionality for le-gpt-chat -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Handles saving and loading chat sessions to/from files.

;;; Code:

(require 'le-gpt-chat)

(defcustom le-gpt-chat-save-directory
  (expand-file-name "le-gpt-chats/" user-emacs-directory)
  "Directory where GPT chat files are saved."
  :type 'directory
  :group 'le-gpt)

;; Private functions
(defun le-gpt-chat--ensure-save-directory ()
  "Ensure the chat save directory exists."
  (unless (file-exists-p le-gpt-chat-save-directory)
    (make-directory le-gpt-chat-save-directory t)))

(defun le-gpt-chat--save-buffer (buffer filename)
  "Save BUFFER content and metadata to FILENAME."
  (with-current-buffer buffer
    (let* ((content (buffer-string))
           (metadata (when (local-variable-p 'buffer-metadata)
                       (buffer-local-value 'buffer-metadata buffer)))
           (data `(:content ,content :metadata ,metadata)))
      (le-gpt-chat--ensure-save-directory)
      (with-temp-file filename
        (prin1 data (current-buffer))))))

(defun le-gpt-chat--load-file (filename)
  "Load a chat from FILENAME and create a new chat buffer."
  (let* ((data (with-temp-buffer
                 (insert-file-contents filename)
                 (read (current-buffer))))
         (content (plist-get data :content))
         (metadata (plist-get data :metadata))
         (name-no-underscores (replace-regexp-in-string "_" " " (file-name-base filename))))
    (let ((buffer (le-gpt--chat-create-buffer name-no-underscores)))
      (with-current-buffer buffer
        (le-gpt-chat-mode)
        (setq-local buffer-metadata metadata)
        (insert content))
      (switch-to-buffer buffer))))

(defun le-gpt-chat--get-default-filename (buffer-name)
  "Generate a default filename from BUFFER-NAME."
  (let* ((name-without-prefix (replace-regexp-in-string "\\*le-gpt\\[[0-9]+\\]: " "" buffer-name))
         (name-without-postfix (substring name-without-prefix 0 -1)) ; remove trailing "*"
         (filename (replace-regexp-in-string "[^a-zA-Z0-9-]" "_" name-without-postfix)))
    (concat (replace-regexp-in-string "_+" "_" filename) ".gpt")))

;; Public functions
(defun le-gpt-chat-save-buffer ()
  "Save the current chat buffer to a file."
  (interactive)
  (unless (derived-mode-p 'le-gpt-chat-mode)
    (user-error "Not in a gpt output buffer"))
  (let* ((default-filename (le-gpt-chat--get-default-filename (buffer-name)))
         (filename (read-file-name "Save chat to: "
                                   le-gpt-chat-save-directory
                                   nil
                                   nil
                                   default-filename)))
    (le-gpt-chat--save-buffer (current-buffer) filename)
    (message "Chat saved to %s" filename)))

(defun le-gpt-chat-load-file ()
  "Load a chat from a file."
  (interactive)
  (let ((filename (read-file-name "Load chat from: "
                                  le-gpt-chat-save-directory
                                  nil
                                  t
                                  nil
                                  (lambda (name)
                                    (or (file-directory-p name)
                                        (string-match-p "\\.gpt$" name))))))
    (le-gpt-chat--load-file filename)))

(provide 'le-gpt-chat-persistence)
;;; le-gpt-chat-persistence.el ends here
