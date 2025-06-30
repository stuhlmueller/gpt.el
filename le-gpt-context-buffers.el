;;; le-gpt-context-buffers.el --- Buffer context handling for le-gpt -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Buffer context handling functionality.

;;; Code:

(require 'project)
(require 'le-gpt-context-lang)

(defun le-gpt--get-buffer-names ()
  "Get list of buffer names, excluding special buffers,
project file buffers, and non-text buffers."
  (let ((project-files (condition-case nil
                           (when (project-current)
                             (let ((project-root (project-root (project-current))))
                               (mapcar (lambda (f) (expand-file-name f project-root))
                                       (project-files (project-current)))))
                         (error nil))))
    (seq-filter (lambda (name)
                  (and
                   ;; Exclude buffers that are project files
                   (let ((buffer (get-buffer name)))
                     (if (and buffer project-files)
                         (let ((buffer-file (buffer-file-name buffer)))
                           (not (and buffer-file
                                     (member buffer-file project-files))))
                       t))
                   ;; Exclude non-text buffers
                   (let ((buffer (get-buffer name)))
                     (when buffer
                       (with-current-buffer buffer
                         (and
                          ;; Not in image-mode or related modes
                          (not (derived-mode-p 'image-mode))
                          ;; Not in doc-view-mode (for PDFs, etc.)
                          (not (derived-mode-p 'doc-view-mode))
                          ;; Not in archive-mode
                          (not (derived-mode-p 'archive-mode))
                          ;; Not in tar-mode
                          (not (derived-mode-p 'tar-mode))
                          ;; Check if buffer contains binary data
                          (not (and (buffer-file-name)
                                    (let ((file-ext (file-name-extension (buffer-file-name))))
                                      (and file-ext
                                           (member (downcase file-ext)
                                                   '("jpg" "jpeg" "png" "gif" "bmp" "tiff" "webp"
                                                     "pdf" "doc" "docx" "xls" "xlsx" "ppt" "pptx"
                                                     "zip" "tar" "gz" "bz2" "7z" "rar"
                                                     "mp3" "mp4" "avi" "mov" "wav" "flac"
                                                     "exe" "dll" "so" "dylib" "bin"))))))))))))
                (mapcar #'buffer-name (buffer-list)))))

(defun le-gpt--get-selected-buffers-contents (selected-context-buffers)
  "Get contents of SELECTED-CONTEXT-BUFFERS
as a formatted string with proper syntax highlighting."
  (let ((buffer-contents ""))
    (dolist (buffer-name selected-context-buffers)
      (condition-case err
          (when-let ((buffer (get-buffer buffer-name)))
            (let ((language (le-gpt--get-language-for-buffer buffer)))
              (setq buffer-contents
                    (concat buffer-contents
                            (format "\nBuffer: %s\n```%s\n%s\n```\n"
                                    buffer-name
                                    language
                                    (with-current-buffer buffer
                                      (buffer-string)))))))
        (error
         (message "Error reading buffer %s: %s" buffer-name (error-message-string err)))))
    buffer-contents))

(defun le-gpt--get-buffer-last-used (buffer-name)
  "Get approximate last used time for BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (or (and (boundp 'buffer-display-time) buffer-display-time)
            (current-time))))))

(provide 'le-gpt-context-buffers)
;;; le-gpt-context-buffers.el ends here
