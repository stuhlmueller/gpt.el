;;; le-gpt-context-history.el --- Context history management for le-gpt -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; History management for context selections.

;;; Code:

(require 'le-gpt-context-utils)

(defvar le-gpt--context-history nil
  "List of previously used context selections.")

(defun le-gpt--save-context-to-history (selected-items)
  "Save SELECTED-ITEMS to context history."
  (let ((context-entry (list :timestamp (current-time)
                             :items selected-items)))
    (push context-entry le-gpt--context-history)
    ;; Keep only last 10 entries
    (setq le-gpt--context-history
          (seq-take le-gpt--context-history 10))))

(defun le-gpt--create-history-completions ()
  "Create completion candidates from context history."
  (let ((completions '()))
    (cl-loop for i from 0
             for entry in le-gpt--context-history
             do (let* ((timestamp (plist-get entry :timestamp))
                       (items (plist-get entry :items))
                       (summary (le-gpt--format-context-summary items))
                       (candidate (propertize
                                   (format "History #%d: %s" (1+ i) summary)
                                   'context-type 'history
                                   'context-items items)))
                  (push (cons candidate `((type . history)
                                          (timestamp . ,timestamp)
                                          (items . ,items)
                                          (summary . ,summary)))
                        completions)))
    completions))

(defun le-gpt--format-context-summary (items)
  "Create a brief summary of context ITEMS."
  (let ((files (seq-filter (lambda (item)
                             (eq (get-text-property 0 'context-type item) 'file)) items))
        (buffers (seq-filter (lambda (item)
                               (eq (get-text-property 0 'context-type item) 'buffer)) items)))
    (let ((file-names (when files
                        (mapcar (lambda (f) (file-name-nondirectory f)) files)))
          (buffer-names buffers)
          (parts '()))

      ;; Add file summary
      (when files
        (let ((file-count (length files)))
          (if (<= file-count 3)
              (push (format "%s" (string-join file-names ", ")) parts)
            (push (format "%s, +%d more files"
                          (string-join (seq-take file-names 2) ", ")
                          (- file-count 2)) parts))))

      ;; Add buffer summary
      (when buffers
        (let ((buffer-count (length buffers)))
          (if (<= buffer-count 3)
              (push (format "%s" (string-join buffer-names ", ")) parts)
            (push (format "%s, +%d more buffers"
                          (string-join (seq-take buffer-names 2) ", ")
                          (- buffer-count 2)) parts))))

      (if parts
          (string-join (nreverse parts) " | ")
        "empty"))))

(provide 'le-gpt-context-history)
;;; le-gpt-context-history.el ends here
