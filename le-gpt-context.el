;;; le-gpt-context.el --- Context functionality for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;

;;; Code:

(require 'le-gpt-core)
(require 'project)

;; HISTORY
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


;; PROJECT CONTEXT

(defvar le-gpt--selected-context-files nil
  "List of project files whose content should be included as context.")

(defvar le-gpt--project-context-format
  "Project Context Files:\n%s\n\nFile Contents:\n%s\n\n"
  "Format string for including project context in prompts.
First %s is replaced with the list of files, second with their contents.")

(defun le-gpt--get-context ()
  "Get the formatted context string based on the selected context."
  (let* ((selected-items (le-gpt--read-multiple-context-items))
         (selected-files (seq-filter (lambda (item)
                                       (eq (get-text-property 0 'context-type item) 'file))
                                     selected-items))
         (selected-buffers (seq-filter (lambda (item)
                                         (eq (get-text-property 0 'context-type item) 'buffer))
                                       selected-items))
         (context-string ""))

    ;; Save to history if we have selections
    (when selected-items
      (le-gpt--save-context-to-history selected-items))

    ;; Add file context
    (when selected-files
      (setq context-string
            (concat context-string
                    (format le-gpt--project-context-format
                            (mapconcat #'identity selected-files "\n")
                            (le-gpt--get-selected-files-contents selected-files)))))

    ;; Add buffer context
    (when selected-buffers
      (setq context-string
            (concat context-string
                    (format "Buffer Context:\n%s\n\nBuffer Contents:\n%s\n\n"
                            (mapconcat #'identity selected-buffers "\n")
                            (le-gpt--get-selected-buffers-contents selected-buffers)))))

    (when (not (string-empty-p context-string))
      context-string)))

(defun le-gpt--create-context-completions ()
  "Create completion candidates for context selection with rich metadata."
  (let ((completions '())
        (project-files (condition-case nil
                           (le-gpt--get-project-files)
                         (error nil)))
        (buffer-names (le-gpt--get-buffer-names)))

    ;; Add buffers with rich metadata (added first, will end up last)
    (dolist (buffer-name buffer-names)
      (let* ((buffer (get-buffer buffer-name))
             (candidate (propertize buffer-name
                                    'context-type 'buffer
                                    'context-name buffer-name))
             (size (when buffer (buffer-size buffer)))
             (modified (when buffer (buffer-modified-p buffer)))
             (file-path (when buffer (buffer-file-name buffer)))
             (mode (when buffer
                     (with-current-buffer buffer
                       major-mode)))
             (lines (when buffer
                      (with-current-buffer buffer
                        (count-lines (point-min) (point-max)))))
             (last-used (le-gpt--get-buffer-last-used buffer-name)))
        (push (cons candidate `((type . buffer)
                                (name . ,buffer-name)
                                (size . ,size)
                                (modified . ,modified)
                                (file-path . ,file-path)
                                (mode . ,mode)
                                (lines . ,lines)
                                (last-used . ,last-used)))
              completions)))

    ;; Add project files with rich metadata (added second, will be middle)
    (dolist (file project-files)
      (let* ((full-path (expand-file-name file (project-root (project-current))))
             (candidate (propertize file
                                    'context-type 'file
                                    'context-path file))
             (size (le-gpt--get-file-size full-path))
             (modified (le-gpt--get-file-modified-time full-path))
             (extension (file-name-extension file))
             (lines (le-gpt--count-file-lines full-path)))
        (push (cons candidate `((type . file)
                                (path . ,file)
                                (full-path . ,full-path)
                                (name . ,file)
                                (size . ,size)
                                (modified . ,modified)
                                (extension . ,extension)
                                (lines . ,lines)))
              completions)))

    ;; Add history entries LAST (will end up first due to append)
    (setq completions (append (le-gpt--create-history-completions) completions))

    completions))

(defun le-gpt--get-file-size (file-path)
  "Get size of FILE-PATH in bytes."
  (condition-case nil
      (file-attribute-size (file-attributes file-path))
    (error 0)))

(defun le-gpt--format-file-size (bytes)
  "Format BYTES as human readable size."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fK" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fM" (/ bytes 1024.0 1024.0)))
   (t (format "%.1fG" (/ bytes 1024.0 1024.0 1024.0)))))

(defun le-gpt--get-file-modified-time (file-path)
  "Get modification time of FILE-PATH."
  (condition-case nil
      (file-attribute-modification-time (file-attributes file-path))
    (error nil)))

(defun le-gpt--format-time-ago (time)
  "Format TIME as relative time ago."
  (if (not time)
      ""
    (let* ((now (current-time))
           (diff (time-subtract now time))
           (seconds (time-to-seconds diff)))
      (cond
       ((< seconds 60) "now")
       ((< seconds 3600) (format "%dm" (/ seconds 60)))
       ((< seconds 86400) (format "%dh" (/ seconds 3600)))
       ((< seconds 604800) (format "%dd" (/ seconds 86400)))
       (t (format "%dw" (/ seconds 604800)))))))

(defun le-gpt--count-file-lines (file-path)
  "Count lines in FILE-PATH efficiently."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file-path)
        (count-lines (point-min) (point-max)))
    (error 0)))

(defun le-gpt--get-buffer-last-used (buffer-name)
  "Get approximate last used time for BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (or (and (boundp 'buffer-display-time) buffer-display-time)
            (current-time))))))


(defun le-gpt--get-context-annotations (completions)
  "Get annotation function with dynamic padding for perfect alignment."
  (let ((max-name-length (apply #'max
                                (mapcar (lambda (comp) (length (car comp)))
                                        completions))))
    (lambda (candidate)
      (let* ((metadata (assoc-default candidate completions))
             (type (assoc-default 'type metadata))
             (name (assoc-default 'name metadata))
             (current-length (length candidate))
             ;; Ensure minimum 4 spaces, but align all entries
             (padding (max 4 (- (+ max-name-length 4) current-length))))
        (cond
         ((eq type 'history)
          (let* ((timestamp (assoc-default 'timestamp metadata))
                 (summary (assoc-default 'summary metadata)))
            (format "%s%-8s %s"
                    (make-string padding ?\s)
                    "[History]"
                    (le-gpt--format-time-ago timestamp))))
         ((eq type 'file)
          (let* ((path (assoc-default 'path metadata))
                 (size (assoc-default 'size metadata))
                 (ext (file-name-extension path))
                 (modified (assoc-default 'modified metadata)))
            (format "%s%-8s %-8s %-8s %s"
                    (make-string padding ?\s)
                    "[File]"
                    (le-gpt--format-file-size size)
                    (if ext (format "(%s)" ext) "")
                    (le-gpt--format-time-ago modified))))
         ((eq type 'buffer)
          (let* ((size (assoc-default 'size metadata))
                 (modified (assoc-default 'modified metadata))
                 (mode (assoc-default 'mode metadata))
                 (file-path (assoc-default 'file-path metadata)))
            (format "%s%-8s %-8s %-12s%s%s"
                    (make-string padding ?\s)
                    "[Buffer]"
                    (if size (le-gpt--format-file-size size) "0B")
                    (if mode
                        (format "(%s)" (replace-regexp-in-string "-mode$" "" (symbol-name mode)))
                      "")
                    (if modified " *mod*" "")
                    (if (and file-path (not (file-exists-p file-path))) " *del*" ""))))
         (t ""))))))

(defun le-gpt--get-context-group (completions)
  "Get group function for context COMPLETIONS with counts."
  (lambda (candidate transform)
    (if transform
        candidate
      (let* ((metadata (assoc-default candidate completions))
             (type (assoc-default 'type metadata))
             (file-count (length (seq-filter (lambda (c) (eq (assoc-default 'type (cdr c)) 'file)) completions)))
             (buffer-count (length (seq-filter (lambda (c) (eq (assoc-default 'type (cdr c)) 'buffer)) completions)))
             (history-count (length (seq-filter (lambda (c) (eq (assoc-default 'type (cdr c)) 'history)) completions))))
        (cond
         ((eq type 'history) (format "🕒 Recent Context (%d)" history-count))
         ((eq type 'file) (format "📄 Project Files (%d)" file-count))
         ((eq type 'buffer) (format "📋 Buffers (%d)" buffer-count))
         (t "Other"))))))

(defun le-gpt--context-collection-fn (completions)
  "Get collection function for context COMPLETIONS."
  (lambda (str pred flag)
    (cl-case flag
      (metadata
       `(metadata
         (annotation-function . ,(le-gpt--get-context-annotations completions))
         (group-function . ,(le-gpt--get-context-group completions))
         (display-sort-function . ,#'identity)))
      (t
       (all-completions str completions pred)))))

(defun le-gpt--read-multiple-context-items ()
  "Let user select multiple context items using proper completion metadata."
  (let* ((completions (le-gpt--create-context-completions))
         (available-choices (mapcar #'car completions))
         (choices available-choices)
         (selection nil)
         (done nil)
         (selected-items '()))

    (while (not done)
      (let ((selected-help (if selected-items
                               (concat "Currently selected:\n"
                                       (mapconcat
                                        (lambda (item)
                                          (let ((type (get-text-property 0 'context-type item)))
                                            (format "  %s %s"
                                                    (if (eq type 'file) "📄" "📋")
                                                    item)))
                                        selected-items "\n")
                                       "\n\n")
                             "No items selected yet\n\n")))

        (setq selection
              (completing-read
               (format "%sSelect context item (empty input when done) [%d selected]: "
                       selected-help
                       (length selected-items))
               (le-gpt--context-collection-fn
                ;; Filter completions to only show remaining choices
                (seq-filter (lambda (comp) (member (car comp) choices)) completions))
               nil nil nil nil ""))

        (if (string-empty-p selection)
            (setq done t)
          ;; Find the original completion item to restore text properties
          (let ((original-item (car (seq-find (lambda (comp)
                                                (string= (car comp) selection))
                                              completions))))
            (when original-item

              (if (eq (get-text-property 0 'context-type original-item) 'history)
                  (progn
                    ;; Use the stored items from history
                    (setq selected-items (get-text-property 0 'context-items original-item))
                    (setq done t))
              (push original-item selected-items)
              (setq choices (delete selection choices))))))))

    (nreverse selected-items)))

(defun le-gpt--get-selected-files-contents (selected-context-files)
  "Get contents of SELECTED-CONTEXT-FILES as a formatted string."
  (let ((file-contents "")
        (project-root (project-root (project-current))))
    (dolist (file selected-context-files)
      (condition-case err
          (setq file-contents
                (concat file-contents
                        (format "\nFile: %s\n```\n%s\n```\n"
                                file
                                (with-temp-buffer
                                  (insert-file-contents
                                   (expand-file-name file project-root))
                                  (let ((content (buffer-string)))
                                    content)))))
        (error
         (message "Error reading file %s: %s" file (error-message-string err)))))
    file-contents))

(defun le-gpt--get-project-files ()
  "Get list of files in current project using project.el."
  (let ((current-project (project-current)))
    (if current-project
        (mapcar (lambda (f)
                  (file-relative-name f (project-root (project-current))))
                (project-files current-project))
      (error "Not in any project recognized by project.el"))))

(defun le-gpt--get-buffer-names ()
  "Get list of buffer names, excluding special buffers."
  (seq-filter (lambda (name)
                (not (string-match-p "^\\*" name)))
              (mapcar #'buffer-name (buffer-list))))

(defun le-gpt--get-selected-buffers-contents (selected-context-buffers)
  "Get contents of SELECTED-CONTEXT-BUFFERS as a formatted string."
  (let ((buffer-contents ""))
    (dolist (buffer-name selected-context-buffers)
      (condition-case err
          (when-let ((buffer (get-buffer buffer-name)))
            (setq buffer-contents
                  (concat buffer-contents
                          (format "\nBuffer: %s\n```\n%s\n```\n"
                                  buffer-name
                                  (with-current-buffer buffer
                                    (buffer-string))))))
        (error
         (message "Error reading buffer %s: %s" buffer-name (error-message-string err)))))
    buffer-contents))

(provide 'le-gpt-context)
;;; le-gpt-context.el ends here
