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

(defun le-gpt--get-project-files ()
  "Get list of files in current project using project.el."
  (let ((current-project (project-current)))
    (if current-project
        (seq-filter #'le-gpt--is-text-file-p
                    (mapcar (lambda (f)
                              (file-relative-name f (project-root (project-current))))
                            (project-files current-project)))
      (error "Not in any project recognized by project.el"))))

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
(defun le-gpt--get-buffer-names ()
  "Get list of buffer names, excluding special buffers, project file buffers, and non-text buffers."
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
  "Get contents of SELECTED-CONTEXT-BUFFERS as a formatted string with proper syntax highlighting."
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

(defun le-gpt--get-language-for-file (file-path)
  "Get the appropriate markdown language identifier for FILE-PATH."
  (let ((ext (downcase (or (file-name-extension file-path) ""))))
    (cond
     ;; Programming languages
     ((member ext '("el" "elisp")) "elisp")
     ((member ext '("py" "python")) "python")
     ((member ext '("js" "javascript")) "javascript")
     ((member ext '("ts" "typescript")) "typescript")
     ((member ext '("jsx")) "jsx")
     ((member ext '("tsx")) "tsx")
     ((member ext '("java")) "java")
     ((member ext '("c")) "c")
     ((member ext '("cpp" "cc" "cxx" "c++")) "cpp")
     ((member ext '("h" "hpp" "hh" "hxx")) "c")
     ((member ext '("cs")) "csharp")
     ((member ext '("php")) "php")
     ((member ext '("rb" "ruby")) "ruby")
     ((member ext '("go")) "go")
     ((member ext '("rs" "rust")) "rust")
     ((member ext '("swift")) "swift")
     ((member ext '("kt" "kotlin")) "kotlin")
     ((member ext '("scala")) "scala")
     ((member ext '("clj" "cljs" "cljc")) "clojure")
     ((member ext '("hs" "haskell")) "haskell")
     ((member ext '("ml" "mli")) "ocaml")
     ((member ext '("fs" "fsx" "fsi")) "fsharp")
     ((member ext '("r")) "r")
     ((member ext '("m" "matlab")) "matlab")
     ((member ext '("pl" "perl")) "perl")
     ((member ext '("lua")) "lua")
     ((member ext '("dart")) "dart")

     ;; Shell and scripting
     ((member ext '("sh" "bash" "zsh" "fish")) "bash")
     ((member ext '("ps1" "powershell")) "powershell")
     ((member ext '("bat" "cmd")) "batch")

     ;; Web technologies
     ((member ext '("html" "htm")) "html")
     ((member ext '("css")) "css")
     ((member ext '("scss" "sass")) "scss")
     ((member ext '("less")) "less")
     ((member ext '("xml")) "xml")
     ((member ext '("svg")) "svg")

     ;; Data formats
     ((member ext '("json")) "json")
     ((member ext '("yaml" "yml")) "yaml")
     ((member ext '("toml")) "toml")
     ((member ext '("ini" "cfg" "conf")) "ini")
     ((member ext '("csv")) "csv")

     ;; Documentation
     ((member ext '("md" "markdown")) "markdown")
     ((member ext '("rst")) "rst")
     ((member ext '("tex" "latex")) "latex")
     ((member ext '("org")) "org")

     ;; Database
     ((member ext '("sql")) "sql")

     ;; Configuration
     ((member ext '("dockerfile")) "dockerfile")
     ((member ext '("makefile")) "makefile")
     ((member ext '("cmake")) "cmake")

     ;; Default to text for unknown extensions
     (t "text"))))

(defun le-gpt--get-language-for-buffer (buffer)
  "Get the appropriate markdown language identifier for BUFFER based on its major mode."
  (when buffer
    (with-current-buffer buffer
      (let ((mode-name (symbol-name major-mode)))
        (cond
         ;; Emacs Lisp
         ((string-match-p "emacs-lisp\\|elisp" mode-name) "elisp")
         ((string-match-p "lisp" mode-name) "lisp")

         ;; Programming languages
         ((string-match-p "python" mode-name) "python")
         ((string-match-p "javascript\\|js2?\\|rjsx" mode-name) "javascript")
         ((string-match-p "typescript\\|tsx?" mode-name) "typescript")
         ((string-match-p "java" mode-name) "java")
         ((string-match-p "c\\+\\+\\|cpp" mode-name) "cpp")
         ((string-match-p "\\bc\\b" mode-name) "c")
         ((string-match-p "csharp\\|c#" mode-name) "csharp")
         ((string-match-p "php" mode-name) "php")
         ((string-match-p "ruby" mode-name) "ruby")
         ((string-match-p "go" mode-name) "go")
         ((string-match-p "rust" mode-name) "rust")
         ((string-match-p "swift" mode-name) "swift")
         ((string-match-p "kotlin" mode-name) "kotlin")
         ((string-match-p "scala" mode-name) "scala")
         ((string-match-p "clojure" mode-name) "clojure")
         ((string-match-p "haskell" mode-name) "haskell")
         ((string-match-p "ocaml" mode-name) "ocaml")
         ((string-match-p "fsharp" mode-name) "fsharp")
         ((string-match-p "r\\|ess" mode-name) "r")
         ((string-match-p "matlab" mode-name) "matlab")
         ((string-match-p "perl" mode-name) "perl")
         ((string-match-p "lua" mode-name) "lua")
         ((string-match-p "dart" mode-name) "dart")

         ;; Shell and scripting
         ((string-match-p "sh\\|shell\\|bash" mode-name) "bash")
         ((string-match-p "powershell" mode-name) "powershell")
         ((string-match-p "bat\\|cmd" mode-name) "batch")

         ;; Web technologies
         ((string-match-p "html\\|web" mode-name) "html")
         ((string-match-p "css" mode-name) "css")
         ((string-match-p "scss\\|sass" mode-name) "scss")
         ((string-match-p "less" mode-name) "less")
         ((string-match-p "xml\\|nxml" mode-name) "xml")

         ;; Data formats
         ((string-match-p "json" mode-name) "json")
         ((string-match-p "yaml" mode-name) "yaml")
         ((string-match-p "toml" mode-name) "toml")
         ((string-match-p "conf\\|ini" mode-name) "ini")

         ;; Documentation
         ((string-match-p "markdown\\|md" mode-name) "markdown")
         ((string-match-p "rst" mode-name) "rst")
         ((string-match-p "latex\\|tex" mode-name) "latex")
         ((string-match-p "org" mode-name) "org")

         ;; Database
         ((string-match-p "sql" mode-name) "sql")

         ;; Configuration
         ((string-match-p "dockerfile" mode-name) "dockerfile")
         ((string-match-p "makefile" mode-name) "makefile")
         ((string-match-p "cmake" mode-name) "cmake")

         ;; Fallback to buffer file extension if available
         ((buffer-file-name)
          (le-gpt--get-language-for-file (buffer-file-name)))

         ;; Default
         (t "text"))))))

(provide 'le-gpt-context)
;;; le-gpt-context.el ends here
