;;; le-gpt-context.el --- Context functionality for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;

;;; Code:

(require 'le-gpt-core)
(require 'project)

;; PROJECT CONTEXT

(defvar le-gpt--selected-context-files nil
  "List of project files whose content should be included as context.")

(defvar le-gpt--project-context-format
  "Project Context Files:\n%s\n\nFile Contents:\n%s\n\n"
  "Format string for including project context in prompts.
First %s is replaced with the list of files, second with their contents.")

(defun le-gpt--get-context ()
  "Get the formatted context string based on the selected context."
  (let ((selected-context-files (le-gpt--read-multiple-files
                                 (le-gpt--get-project-files)))
        (selected-context-buffers (le-gpt--read-multiple-buffers
                                   (le-gpt--get-buffer-names)))
        (context-string ""))
    
    ;; Add file context
    (when selected-context-files
      (setq context-string
            (concat context-string
                    (format le-gpt--project-context-format
                            (mapconcat #'identity selected-context-files "\n")
                            (le-gpt--get-selected-files-contents selected-context-files)))))
    
    ;; Add buffer context
    (when selected-context-buffers
      (setq context-string
            (concat context-string
                    (format "Buffer Context:\n%s\n\nBuffer Contents:\n%s\n\n"
                            (mapconcat #'identity selected-context-buffers "\n")
                            (le-gpt--get-selected-buffers-contents selected-context-buffers)))))
    
    (when (not (string-empty-p context-string))
      context-string)))

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

(defun le-gpt--read-multiple-files (files)
  "Let user select multiple FILES using completion.
Shows currently selected files.  Empty input finishes selection."
  (let ((choices files)
        (selection nil)
        (done nil)
        (selected-files le-gpt--selected-context-files))
    (while (not done)
      (let ((selected-help (if selected-files
                               (concat "Currently selected:\n"
                                       (mapconcat (lambda (f) (concat "  - " f))
                                                  selected-files "\n")
                                       "\n\n")
                             "No files selected yet\n\n")))
        (setq selection
              (completing-read
               (format "%sSelect file (empty input when done) [%d selected]: "
                       selected-help
                       (length selected-files))
               choices nil nil nil nil ""))

        (if (string-empty-p selection)
            (setq done t)
          (push selection selected-files)
          (setq choices (delete selection choices)))))
    (nreverse selected-files)))

(defun le-gpt--get-project-files ()
  "Get list of files in current project using project.el."
  (let ((current-project (project-current)))
    (if current-project
        (mapcar (lambda (f)
                  (file-relative-name f (project-root (project-current))))
                (project-files current-project))
      (error "Not in any project recognized by project.el"))))

(defun le-gpt--read-multiple-files-to-remove (files)
  "Let user select multiple FILES to remove using completion.
Shows files selected for removal.  Empty input finishes selection."
  (let ((choices files)
        (selection nil)
        (done nil)
        (files-to-remove '()))
    (while (not done)
      (let ((remove-help (if files-to-remove
                             (concat "Selected for removal:\n"
                                     (mapconcat (lambda (f) (concat "  - " f))
                                                files-to-remove "\n")
                                     "\n\n")
                           "No files selected for removal yet\n\n")))
        (setq selection
              (completing-read
               (format "%sSelect file to remove (empty to finish) [%d to remove]: "
                       remove-help
                       (length files-to-remove))
               choices nil nil))

        (if (string-empty-p selection)
            (setq done t)
          (push selection files-to-remove)
          (setq choices (delete selection choices)))))
    (nreverse files-to-remove)))

;; BUFFER CONTEXT
(defun le-gpt--get-buffer-names ()
  "Get list of buffer names, excluding special buffers."
  (seq-filter (lambda (name)
                (not (string-match-p "^\\*" name)))
              (mapcar #'buffer-name (buffer-list))))

(defun le-gpt--read-multiple-buffers (buffers)
  "Let user select multiple BUFFERS using completion."
  (let ((choices buffers)
        (selection nil)
        (done nil)
        (selected-buffers '()))
    (while (not done)
      (let ((selected-help (if selected-buffers
                               (concat "Currently selected buffers:\n"
                                       (mapconcat (lambda (b) (concat "  - " b))
                                                  selected-buffers "\n")
                                       "\n\n")
                             "No buffers selected yet\n\n")))
        (setq selection
              (completing-read
               (format "%sSelect buffer (empty input when done) [%d selected]: "
                       selected-help
                       (length selected-buffers))
               choices nil nil nil nil ""))

        (if (string-empty-p selection)
            (setq done t)
          (push selection selected-buffers)
          (setq choices (delete selection choices)))))
    (nreverse selected-buffers)))

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
