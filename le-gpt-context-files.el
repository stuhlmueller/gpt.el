;;; le-gpt-context-files.el --- File context handling for le-gpt -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; File and project context handling functionality.

;;; Code:

(require 'project)
(require 'le-gpt-context-lang)
(require 'le-gpt-context-utils)

(defvar le-gpt--selected-context-files nil
  "List of project files whose content should be included as context.")

(defvar le-gpt--project-context-format
  "Project Context Files:\n%s\n\nFile Contents:\n%s\n\n"
  "Format string for including project context in prompts.
First %s is replaced with the list of files, second with their contents.")

(defun le-gpt--get-project-files ()
  "Get list of files in current project using project.el."
  (let ((current-project (project-current)))
    (if current-project
        (seq-filter #'le-gpt--is-text-file-p
                    (mapcar (lambda (f)
                              (file-relative-name f (project-root (project-current))))
                            (project-files current-project)))
      (error "Not in any project recognized by project.el"))))

(defun le-gpt--get-selected-files-contents (selected-context-files)
  "Get contents of SELECTED-CONTEXT-FILES as a formatted string with proper syntax highlighting."
  (let ((file-contents "")
        (project-root (project-root (project-current))))
    (dolist (file selected-context-files)
      (condition-case err
          (let* ((full-path (expand-file-name file project-root))
                 (language (le-gpt--get-language-for-file full-path)))
            (setq file-contents
                  (concat file-contents
                          (format "\nFile: %s\n```%s\n%s\n```\n"
                                  file
                                  language
                                  (with-temp-buffer
                                    (insert-file-contents full-path)
                                    (buffer-string))))))
        (error
         (message "Error reading file %s: %s" file (error-message-string err)))))
    file-contents))

(defun le-gpt--is-text-file-p (file-path)
  "Return t if FILE-PATH is likely a text file that can be properly encoded."
  (let* ((full-path (if (file-name-absolute-p file-path)
                        file-path
                      (expand-file-name file-path (project-root (project-current)))))
         (ext (downcase (or (file-name-extension full-path) ""))))
    (and
     ;; Check file exists and is readable
     (file-readable-p full-path)
     ;; Not a directory
     (not (file-directory-p full-path))
     ;; Exclude binary file extensions
     (not (member ext '("jpg" "jpeg" "png" "gif" "bmp" "tiff" "webp" "ico"
                        "pdf" "doc" "docx" "xls" "xlsx" "ppt" "pptx" "odt" "ods" "odp"
                        "zip" "tar" "gz" "bz2" "7z" "rar" "xz" "lz" "lzma"
                        "mp3" "mp4" "avi" "mov" "mkv" "wav" "flac" "ogg" "m4a"
                        "exe" "dll" "so" "dylib" "bin" "deb" "rpm" "dmg" "msi"
                        "class" "jar" "war" "ear" "pyc" "pyo" "o" "obj"
                        "ttf" "otf" "woff" "woff2" "eot"
                        "sqlite" "db" "mdb")))
     ;; Additional check for files without extensions or with text-like extensions
     (or (member ext '("txt" "md" "org" "rst" "tex" "log" "cfg" "conf" "ini"
                       "json" "xml" "yaml" "yml" "toml" "csv" "tsv"
                       "html" "htm" "css" "js" "ts" "jsx" "tsx" "vue" "svelte"
                       "py" "rb" "php" "java" "c" "cpp" "h" "hpp" "cs" "go" "rs"
                       "el" "lisp" "clj" "hs" "ml" "scala" "kt" "swift" "dart"
                       "sh" "bash" "zsh" "fish" "ps1" "bat" "cmd"
                       "sql" "r" "m" "pl" "lua" "vim" "dockerfile" "makefile"
                       "gitignore" "gitattributes" "editorconfig" "eslintrc"
                       "prettierrc" "babelrc" "npmrc" "yarnrc"))
         ;; For files without extension, do a basic content check
         (and (string-empty-p ext)
              (le-gpt--file-appears-textual-p full-path))))))

(defun le-gpt--file-appears-textual-p (file-path)
  "Check if FILE-PATH appears to contain text by examining first few bytes."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file-path nil 0 1024)
        (let ((content (buffer-string)))
          ;; Check if content is mostly printable ASCII/UTF-8
          (and (> (length content) 0)
               (< (/ (length (seq-filter (lambda (c) (or (< c 32) (> c 126))) content))
                     (float (length content)))
                  0.3)))) ; Allow up to 30% non-printable characters
    (error nil)))

(defun le-gpt--get-file-size (file-path)
  "Get size of FILE-PATH in bytes."
  (condition-case nil
      (file-attribute-size (file-attributes file-path))
    (error 0)))

(defun le-gpt--get-file-modified-time (file-path)
  "Get modification time of FILE-PATH."
  (condition-case nil
      (file-attribute-modification-time (file-attributes file-path))
    (error nil)))

(defun le-gpt--count-file-lines (file-path)
  "Count lines in FILE-PATH efficiently."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file-path)
        (count-lines (point-min) (point-max)))
    (error 0)))

(provide 'le-gpt-context-files)
;;; le-gpt-context-files.el ends here
