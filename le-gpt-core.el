;;; le-gpt-core.el --- Core functionality for le-gpt.el -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'savehist)
(require 'project)

;; Core configuration variables
(defvar le-gpt--script-path
  (expand-file-name "le-gpt.py" (file-name-directory (or load-file-name buffer-file-name)))
  "The path to the Python script used by gpt.el.")

(defcustom le-gpt-model "gpt-4o"
  "The model to use (e.g., `gpt-4', `claude-3-5-sonnet-20240620')."
  :type 'string
  :group 'le-gpt)

(defcustom le-gpt-max-tokens 2000
  "The max_tokens value used with the chosen model."
  :type 'integer
  :group 'le-gpt)

(defcustom le-gpt-temperature 0
  "The temperature value used with the chosen model."
  :type 'float
  :group 'le-gpt)

(defcustom le-gpt-openai-key "NOT SET"
  "The OpenAI API key to use."
  :type 'string
  :group 'le-gpt)

(defcustom le-gpt-anthropic-key "NOT SET"
  "The Anthropic API key to use."
  :type 'string
  :group 'le-gpt)

(defcustom le-gpt-api-type 'openai
  "The type of API to use.  Either \='openai or \='anthropic."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Anthropic" anthropic))
  :group 'le-gpt)

(defcustom le-gpt-python-path "python"
  "The path to your python executable."
  :type 'string
  :group 'le-gpt)


;; Core process management functions
(defun le-gpt--make-process (prompt-file output-buffer)
  "Create a GPT process with PROMPT-FILE, and OUTPUT-BUFFER.
Use `le-gpt-python-path' and `le-gpt--script-path' to execute
the command with necessary arguments."
  (let* ((api-key (if (eq le-gpt-api-type 'openai) le-gpt-openai-key le-gpt-anthropic-key))
         (api-type-str (symbol-name le-gpt-api-type))
         (process (make-process
                   :name "le-gpt-process"
                   :buffer output-buffer
                   :command (list le-gpt-python-path
                                  le-gpt--script-path
                                  prompt-file api-key le-gpt-model
                                  (number-to-string le-gpt-max-tokens)
                                  (number-to-string le-gpt-temperature) api-type-str)
                   :connection-type 'pipe))
         (timer (le-gpt--start-timer process)))
    (le-gpt--set-process-sentinel process timer prompt-file)
    process))

(defun le-gpt--start-timer (process)
  "Set timer to run every second and print message if PROCESS is still running."
  (run-with-timer 1 1
                  (lambda (timer-object)
                    (when (process-live-p timer-object)
                      (font-lock-update)
                      (message "GPT Pilot: Running...")))
                  process))

(defun le-gpt--set-process-sentinel (process timer prompt-file)
  "Set a function to run when the PROCESS finishes or fails.
TIMER is the timer object that cancels the process after a timeout.
PROMPT-FILE is the temporary file containing the prompt."
  (set-process-sentinel
   process
   (lambda (proc status)
     (when (memq (process-status proc) '(exit signal))
       (cancel-timer timer)
       (if (zerop (process-exit-status proc))
           (progn
             (delete-file prompt-file)
             (message "GPT Pilot: Finished successfully."))
         (message "GPT Pilot: Failed: %s" status))))))

;; Core utility functions
(defun le-gpt--create-prompt-file (input)
  "Create a temporary file containing the prompt string from INPUT."
  (let ((temp-file (make-temp-file "le-gpt-prompt"))
        (content (if (bufferp input)
                     (with-current-buffer input (buffer-string))
                   input)))
    (with-temp-file temp-file
      (insert content))
    (message "GPT Pilot: Prompt written to %s" temp-file)
    temp-file))


(defun le-gpt--completing-read-space (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer with completion, treating space literally.
Arguments PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD have same meaning as in `completing-read'."
  (let ((minibuffer-local-completion-map
         (let ((map (copy-keymap minibuffer-local-completion-map)))
           (define-key map " " 'self-insert-command)
           map)))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))

;; Model switching functionality
(defun le-gpt-switch-model ()
  "Switch between OpenAI and Anthropic models."
  (interactive)
  (let* ((models '(("GPT-4o" . (openai . "gpt-4o"))
                   ("Claude 3.5 Sonnet" . (anthropic . "claude-3-5-sonnet-20240620"))))
         (choice (completing-read "Choose model: " (mapcar #'car models) nil t))
         (model-info (cdr (assoc choice models))))
    (setq le-gpt-api-type (car model-info)
          le-gpt-model (cdr model-info))
    (message "Switched to %s model: %s" (car model-info) (cdr model-info))))

;; Command reading
(defun le-gpt--read-command ()
  "Read a GPT command from the user with history and completion."
  (let ((cmd (le-gpt--completing-read-space "Command: " le-gpt--command-history nil nil nil 'le-gpt--command-history)))
    (if (string-equal cmd "n/a")
        ""
      (string-trim cmd))))

;; Command history management
(defvar le-gpt--command-history nil
  "A list of GPT commands that have been entered by the user.")

(add-to-list 'savehist-additional-variables 'le-gpt--command-history)

(defun le-gpt-display-command-history ()
  "Display the `le-gpt--command-history' in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*GPT Command History*")
    (erase-buffer)
    (insert (mapconcat #'identity le-gpt--command-history "\n"))
    (switch-to-buffer (current-buffer))))

(defun le-gpt-clear-command-history ()
  "Clear the `le-gpt--command-history' list."
  (interactive)
  (setq le-gpt--command-history nil)
  (message "GPT command history cleared."))

(defun le-gpt-export-command-history (file)
  "Export the `le-gpt--command-history' to FILE."
  (interactive "Export le-gpt--command-history to file: ")
  (with-temp-file file
    (dolist (cmd le-gpt--command-history)
      (insert (format "%s\n" cmd)))))

(provide 'le-gpt-core)

;;; le-gpt-core.el ends here
