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
  (let* ((selected-items (le-gpt--read-multiple-context-items))
         (selected-files (seq-filter (lambda (item) 
                                      (eq (get-text-property 0 'context-type item) 'file)) 
                                    selected-items))
         (selected-buffers (seq-filter (lambda (item) 
                                        (eq (get-text-property 0 'context-type item) 'buffer)) 
                                      selected-items))
         (context-string ""))
    
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
  "Create completion candidates for context selection."
  (let ((completions '())
        (project-files (condition-case nil
                          (le-gpt--get-project-files)
                        (error nil)))
        (buffer-names (le-gpt--get-buffer-names)))
    
    ;; Add project files
    (dolist (file project-files)
      (let ((candidate (propertize file 
                                  'context-type 'file
                                  'context-path file)))
        (push (cons candidate `((type . file)
                               (path . ,file)
                               (name . ,file))) completions)))
    
    ;; Add buffers
    (dolist (buffer buffer-names)
      (let ((candidate (propertize buffer 
                                  'context-type 'buffer
                                  'context-name buffer)))
        (push (cons candidate `((type . buffer)
                               (name . ,buffer))) completions)))
    
    completions))

(defun le-gpt--get-context-annotations (completions)
  "Get annotation function for context COMPLETIONS."
  (lambda (candidate)
    (let* ((metadata (assoc-default candidate completions))
           (type (assoc-default 'type metadata)))
      (cond
       ((eq type 'file) " [Project File]")
       ((eq type 'buffer) " [Buffer]")
       (t "")))))

(defun le-gpt--get-context-group (completions)
  "Get group function for context COMPLETIONS."
  (lambda (candidate transform)
    (if transform
        candidate
      (let* ((metadata (assoc-default candidate completions))
             (type (assoc-default 'type metadata)))
        (cond
         ((eq type 'file) "📄 Project Files")
         ((eq type 'buffer) "📋 Buffers")
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
          (push selection selected-items)
          (setq choices (delete selection choices)))))
    
    (nreverse selected-items)))

;; Keep existing helper functions
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
