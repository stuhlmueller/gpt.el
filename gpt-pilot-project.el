;;; gpt-pilot-project.el --- Project context functionality for gpt-pilot.el -*- lexical-binding: t; -*-
(require 'gpt-pilot-core)
(require 'project)

(defvar gpt-pilot--selected-context-files nil
  "List of project files whose content should be included as context.")

(defvar gpt-pilot--project-context-format
  "Project Context Files:\n%s\n\nFile Contents:\n%s\n\n"
  "Format string for including project context in prompts.
First %s is replaced with the list of files, second with their contents.")

(defun gpt-pilot-clear-selected-context-files ()
  "Clear the list of project files used as context."
  (interactive)
  (setq gpt-pilot--selected-context-files nil)
  (message "Project file context cleared."))

(defun gpt-pilot-select-project-files ()
  "Prompt user to select files from project to use as context."
  (interactive)
  (let* ((all-files (gpt-pilot--get-project-files))
         (relative-files (mapcar (lambda (f)
                                   (file-relative-name f (project-root (project-current))))
                                 all-files))
         ;; Remove already selected files from choices
         (available-files (seq-difference relative-files gpt-pilot--selected-context-files)))
    (when-let ((new-selections (gpt-pilot--read-multiple-files available-files)))
      (setq gpt-pilot--selected-context-files
            (append gpt-pilot--selected-context-files new-selections))
      (message "Added %d files. Total files selected: %d"
               (length new-selections)
               (length gpt-pilot--selected-context-files)))))

(defun gpt-pilot-deselect-project-files ()
  "Remove multiple files from the current project context."
  (interactive)
  (if (null gpt-pilot--selected-context-files)
      (message "No files currently selected")
    (let ((to-remove (gpt-pilot--read-multiple-files-to-remove gpt-pilot--selected-context-files)))
      (setq gpt-pilot--selected-context-files
            (seq-difference gpt-pilot--selected-context-files to-remove))
      (message "Removed %d files. Remaining files: %d"
               (length to-remove)
               (length gpt-pilot--selected-context-files)))))

(defun gpt-pilot-get-project-context ()
  "Get the formatted context string based on the selected project files."
  (when gpt-pilot--selected-context-files
    (format gpt-pilot-project-context-format
            (mapconcat #'identity gpt-pilot--selected-context-files "\n")
            (gpt-pilot--get-selected-files-contents))))

(defun gpt-pilot--get-selected-files-contents ()
  "Get contents of selected context files as a formatted string."
  (let ((file-contents "")
        (project-root (project-root (project-current))))
    (dolist (file gpt-pilot--selected-context-files)
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

(defun gpt-pilot--read-multiple-files (files)
  "Let user select multiple FILES using completion.
Shows currently selected files. Empty input finishes selection."
  (let ((choices files)
        (selection nil)
        (done nil)
        (selected-files gpt-pilot--selected-context-files))
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
               choices nil nil))

        (if (string-empty-p selection)
            (setq done t)
          (push selection selected-files)
          (setq choices (delete selection choices)))))
    (nreverse selected-files)))

(defun gpt-pilot--get-project-files ()
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


(defun gpt-pilot--read-multiple-files-to-remove (files)
  "Let user select multiple FILES to remove using completion.
Shows files selected for removal. Empty input finishes selection."
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

(provide 'gpt-pilot-project)
;;; gpt-pilot-project.el ends here
