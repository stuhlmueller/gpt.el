;;; gpt-pilot-project.el --- Project context functionality for gpt-pilot.el -*- lexical-binding: t; -*-
(require 'gpt-pilot-core)
(require 'project)

(defvar gpt-pilot-project-file-context nil
  "List of project files whose content should be included as context.")

(defvar gpt-pilot-project-context-format
  "Project Context Files:\n%s\n\nFile Contents:\n%s\n\n"
  "Format string for including project context in prompts.
First %s is replaced with the list of files, second with their contents.")

(defun gpt-pilot-clear-project-file-context ()
  "Clear the list of project files used as context."
  (interactive)
  (setq gpt-pilot-project-file-context nil)
  (message "Project file context cleared."))

(defun gpt-pilot-get-project-files ()
  "Get list of files in current project using project.el or projectile.el."
  (cond
   ((and (fboundp 'project-current)
         (project-current))
    (project-files (project-current)))
   ((and (fboundp 'projectile-project-root)
         (projectile-project-root))
    (projectile-project-files))
   (t
    (error "No project found. Please use project.el or projectile.el"))))

(defun gpt-pilot-read-multiple-files (files)
  "Let user select multiple FILES using completion.
Return list of selected files."
  (let ((choices files)
        (selection nil)
        (done nil)
        (selected-files '()))
    (while (not done)
      (setq selection
            (completing-read
             (format "Select file [%d selected, RET when done]: "
                     (length selected-files))
             choices nil nil))
      (if (string-empty-p selection)
          (setq done t)
        (push selection selected-files)
        (setq choices (delete selection choices))))
    (nreverse selected-files)))

(defun gpt-pilot-select-project-files ()
  "Prompt user to select files from project to use as context."
  (interactive)
  (let* ((all-files (gpt-pilot-get-project-files))
         (relative-files (mapcar (lambda (f)
                                   (file-relative-name f (project-root (project-current))))
                                 all-files)))
    (setq gpt-pilot-project-file-context
          (gpt-pilot-read-multiple-files relative-files))
    (message "Selected %d files for context" (length gpt-pilot-project-file-context))))

(defun gpt-pilot-get-file-contents (files)
  "Get contents of FILES as a formatted string."
  (let ((file-contents "")
        (project-root (project-root (project-current))))
    (dolist (file files)
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

(provide 'gpt-pilot-project)

;;; gpt-pilot-project.el ends here
