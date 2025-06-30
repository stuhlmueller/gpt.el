;;; le-gpt-context-lang.el --- Language detection for le-gpt context -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Language detection and syntax highlighting support for context files and buffers.

;;; Code:

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
  "Get the appropriate markdown language identifier
for BUFFER based on its major mode."
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

(provide 'le-gpt-context-lang)
;;; le-gpt-context-lang.el ends here
