;;; le-gpt-buffer-list.el --- Buffer list management for le-gpt-chat -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides a buffer list interface for managing GPT chat buffers.

;;; Code:

(require 'le-gpt-chat)
(require 'le-gpt-chat-persistence)

(defvar le-gpt-buffer-list--filter-string ""
  "Current filter string for GPT buffer list.")

;; Mode definition
(defvar le-gpt-buffer-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'le-gpt-buffer-list-open-buffer)
    (define-key map (kbd "d") #'le-gpt-buffer-list-mark-delete)
    (define-key map (kbd "u") #'le-gpt-buffer-list-unmark)
    (define-key map (kbd "x") #'le-gpt-buffer-list-execute)
    (define-key map (kbd "C-c C-t") 'le-gpt-buffer-list-generate-buffer-name)
    (define-key map (kbd "C-c C-s") 'le-gpt-buffer-list-save-buffer)
    (define-key map (kbd "/") #'le-gpt-buffer-list-filter)
    (define-key map (kbd "C-c C-c") #'le-gpt-buffer-list-clear-filter)
    (define-key map (kbd "g r") #'le-gpt-buffer-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `le-gpt-buffer-list-mode'.")

(define-derived-mode le-gpt-buffer-list-mode tabulated-list-mode "Le-GPT Buffers"
  "Major mode for listing GPT buffers."
  :group 'le-gpt
  (setq truncate-lines t)
  (setq tabulated-list-format `[("M" 1 t)
                                ("Buffer name" ,le-gpt-chat-buffer-name-length t)
                                ("Model" 20 t)
                                ("Created at" 20 t)])
  (tabulated-list-init-header))

;; Private functions
(defun le-gpt--buffer-matches-filter-p (buffer filter)
  "Return t if BUFFER content matches FILTER regex."
  (if (string-empty-p filter)
      t
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (re-search-forward filter nil t)))))

(defun le-gpt-buffer-list-get-buffer-name ()
  "Get the buffer name from the current line."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^.\\{2\\}\\(\\*le-gpt\\[[0-9]+\\].*\\*\\)" (line-end-position) t)
      (match-string 1))))

;; Public functions
(defun le-gpt-list-buffers ()
  "Display a list of all GPT buffers."
  (interactive)
  (let ((buf (get-buffer-create "*Le GPT Buffers*")))
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
        (all-gpt-buffers (le-gpt--get-gpt-buffers))
        (filtered-buffers (if (string-empty-p le-gpt-buffer-list--filter-string)
                              (le-gpt--get-gpt-buffers)
                            (seq-filter (lambda (buf)
                                          (le-gpt--buffer-matches-filter-p
                                           buf le-gpt-buffer-list--filter-string))
                                        (le-gpt--get-gpt-buffers)))))
    (erase-buffer)
    (when (not (string-empty-p le-gpt-buffer-list--filter-string))
      (insert (propertize (format "Filter: %s (%d/%d buffers)\n"
                                  le-gpt-buffer-list--filter-string
                                  (length filtered-buffers)
                                  (length all-gpt-buffers))
                          'face 'font-lock-comment-face))
      (insert "\n"))
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
                  filtered-buffers))
    (tabulated-list-print t)))

(defun le-gpt-buffer-list-open-buffer ()
  "Open the GPT buffer at point."
  (interactive)
  (let ((buffer-name (le-gpt-buffer-list-get-buffer-name)))
    (when buffer-name
      (select-window (display-buffer (get-buffer buffer-name)
                                     '(display-buffer-same-window))))))

(defun le-gpt-buffer-list-generate-buffer-name ()
  "Generate the name for the GPT buffer at point."
  (interactive)
  (let ((buffer-name (le-gpt-buffer-list-get-buffer-name)))
    (when buffer-name
      (le-gpt-chat--generate-buffer-name (get-buffer buffer-name))
      (le-gpt-buffer-list-refresh))))

(defun le-gpt-buffer-list-save-buffer ()
  "Save the GPT buffer at point to a file."
  (interactive)
  (let* ((buffer-name (le-gpt-buffer-list-get-buffer-name))
         (buffer (when buffer-name (get-buffer buffer-name))))
    (when buffer
      (let* ((default-filename (le-gpt-chat--get-default-filename buffer-name))
             (filename (read-file-name "Save chat to: "
                                       le-gpt-chat-save-directory
                                       nil
                                       nil
                                       default-filename)))
        (le-gpt-chat--save-buffer buffer filename)
        (message "Chat saved to %s" filename)))))

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

(defun le-gpt-buffer-list-filter ()
  "Filter GPT buffers by regex search on their contents."
  (interactive)
  (let ((filter (read-string "Filter buffers by content (regex): "
                             le-gpt-buffer-list--filter-string)))
    (setq le-gpt-buffer-list--filter-string filter)
    (le-gpt-buffer-list-refresh)))

(defun le-gpt-buffer-list-clear-filter ()
  "Clear the current filter."
  (interactive)
  (setq le-gpt-buffer-list--filter-string "")
  (le-gpt-buffer-list-refresh)
  (message "Filter cleared"))

(provide 'le-gpt-buffer-list)
;;; le-gpt-buffer-list.el ends here
