;;; gpt-pilot-core.el --- Core functionality for gpt-pilot.el -*- lexical-binding: t; -*-
(require 'savehist)
(require 'project)

;; Core configuration variables
(defvar gpt-pilot-script-path
  (expand-file-name "gpt-pilot.py" (file-name-directory (or load-file-name buffer-file-name)))
  "The path to the Python script used by gpt.el.")

(defvar gpt-pilot-model "gpt-4o"
  "The model to use (e.g., 'gpt-4', 'claude-3-5-sonnet-20240620').")

(defvar gpt-pilot-max-tokens "2000"
  "The max_tokens value used with the chosen model.")

(defvar gpt-pilot-temperature "0"
  "The temperature value used with the chosen model.")

(defvar gpt-pilot-openai-key "NOT SET"
  "The OpenAI API key to use.")

(defvar gpt-pilot-anthropic-key "NOT SET"
  "The Anthropic API key to use.")

(defvar gpt-pilot-api-type 'openai
  "The type of API to use. Either 'openai or 'anthropic.")

(defvar gpt-pilot-python-path "python"
  "The path to your python executable.")

;; Core process management functions
(defun gpt-pilot-make-process (prompt-file output-buffer)
  "Create a GPT process with PROMPT-FILE, and OUTPUT-BUFFER.
Use `gpt-pilot-python-path' and `gpt-pilot-script-path' to execute the command with necessary arguments."
  (let* ((api-key (if (eq gpt-pilot-api-type 'openai) gpt-pilot-openai-key gpt-pilot-anthropic-key))
         (api-type-str (symbol-name gpt-pilot-api-type))
         (process (make-process
                   :name "gpt-pilot-process"
                   :buffer output-buffer
                   :command (list gpt-pilot-python-path gpt-pilot-script-path prompt-file api-key gpt-pilot-model gpt-pilot-max-tokens gpt-pilot-temperature api-type-str)
                   :connection-type 'pipe))
         (timer (gpt-pilot-start-timer process)))
    (gpt-pilot-set-process-sentinel process timer prompt-file)
    process))

(defun gpt-pilot-start-timer (process)
  "Set timer to run every second and print message if PROCESS is still running."
  (run-with-timer 1 1
                  (lambda (timer-object)
                    (when (process-live-p timer-object)
                      (font-lock-fontify-buffer)
                      (message "GPT Pilot: Running...")))
                  process))

(defun gpt-pilot-set-process-sentinel (process timer prompt-file)
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
(defun gpt-pilot-create-prompt-file (input)
  "Create a temporary file containing the prompt string from INPUT."
  (let ((temp-file (make-temp-file "gpt-pilot-prompt"))
        (content (if (bufferp input)
                     (with-current-buffer input (buffer-string))
                   input)))
    (with-temp-file temp-file
      (insert content))
    (message "GPT Pilot: Prompt written to %s" temp-file)
    temp-file))

(defun gpt-pilot-buffer-string (buffer)
  "Get BUFFER text as string."
  (with-current-buffer buffer
    (buffer-string)))

(defun gpt-pilot-completing-read-space (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer with completion, treating space literally.
Arguments PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD have same meaning as in `completing-read'."
  (let ((minibuffer-local-completion-map
         (let ((map (copy-keymap minibuffer-local-completion-map)))
           (define-key map " " 'self-insert-command)
           map)))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))

;; Model switching functionality
(defun gpt-pilot-switch-model ()
  "Switch between OpenAI and Anthropic models."
  (interactive)
  (let* ((models '(("GPT-4o" . (openai . "gpt-4o"))
                   ("Claude 3.5 Sonnet" . (anthropic . "claude-3-5-sonnet-20240620"))))
         (choice (completing-read "Choose model: " (mapcar #'car models) nil t))
         (model-info (cdr (assoc choice models))))
    (setq gpt-pilot-api-type (car model-info)
          gpt-pilot-model (cdr model-info))
    (message "Switched to %s model: %s" (car model-info) (cdr model-info))))

;; Command reading
(defun gpt-pilot-read-command ()
  "Read a GPT command from the user with history and completion."
  (let ((cmd (gpt-pilot-completing-read-space "Command: " gpt-pilot-command-history nil nil nil 'gpt-pilot-command-history)))
    (if (string-equal cmd "n/a")
        ""
      (string-trim cmd))))

;; Command history management
(defvar gpt-pilot-command-history nil
  "A list of GPT commands that have been entered by the user.")

(add-to-list 'savehist-additional-variables 'gpt-pilot-command-history)

(defun gpt-pilot-display-command-history ()
  "Display the `gpt-pilot-command-history' in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*GPT Command History*")
    (erase-buffer)
    (insert (mapconcat #'identity gpt-pilot-command-history "\n"))
    (switch-to-buffer (current-buffer))))

(defun gpt-pilot-clear-command-history ()
  "Clear the `gpt-pilot-command-history' list."
  (interactive)
  (setq gpt-pilot-command-history nil)
  (message "GPT command history cleared."))

(defun gpt-pilot-export-command-history (file)
  "Export the `gpt-pilot-command-history' to FILE."
  (interactive "Export gpt-pilot-command-history to file: ")
  (with-temp-file file
    (dolist (cmd gpt-pilot-command-history)
      (insert (format "%s\n" cmd)))))

(provide 'gpt-pilot-core)

;;; gpt-pilot-coreel ends here
