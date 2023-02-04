;;; gpt.el --- Run instruction-following language models -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <andreas@ought.org>
;; Version: 1.0
;; Keywords: gpt3, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This package defines a set of functions and variables for running
;; instruction-following language models like GPT-3.  It allows the
;; user to enter a command with history and completion, and optionally
;; use the current region as input.  The output of the command is
;; displayed in a temporary buffer with the same major mode as the
;; original buffer.  The output is streamed as it is produced by the
;; GPT process.  The user can enter a follow-up command in the output
;; buffer, which will provide the output, the follow-up command to GPT
;; as a new prompt.  The follow-up output will be appended to the
;; output buffer.  The user can view and export the command history to
;; a file.



;;; Code:

(require 'savehist)

(savehist-mode 1)

(defvar gpt-command-history nil
  "A list of commands that have been entered by the user for `gpt-on-region'.")

(defvar gpt-script-path (expand-file-name "gpt.py" (file-name-directory (or load-file-name buffer-file-name)))
  "The path to the Python script used by gpt.el.")

(defvar gpt-openai-engine "text-davinci-003"
  "The OpenAI engine to use.")

(defvar gpt-openai-max-tokens "2000"
  "The max_tokens value used with OpenAI engine.")

(defvar gpt-openai-temperature "0"
  "The temperature value used with OpenAI completion engine.")

(defvar gpt-openai-key "NOT SET"
  "The OpenAI API key to use.")

(add-to-list 'savehist-additional-variables 'gpt-command-history)

(defun gpt-display-command-history ()
  "Display the `gpt-command-history' in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*GPT Command History*")
    (erase-buffer)
    (insert (mapconcat #'identity gpt-command-history "\n"))
    (switch-to-buffer (current-buffer))))

(defun gpt-export-history (file)
  "Export the `gpt-command-history' to FILE."
  (interactive "Export gpt-command-history to file: ")
  (with-temp-file file
    (dolist (cmd gpt-command-history)
      (insert (format "%s\n" cmd)))))

(defun gpt-completing-read-space (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer with completion, treating space literally.

The arguments are the same as for `completing-read', except that
space does not trigger completion or cycling, but inserts a space
character.  PROMPT is the prompt to display, COLLECTION is the
list of possible completions, and the optional arguments PREDICATE
REQUIRE-MATCH INITIAL-INPUT HIST DEF and INHERIT-INPUT-METHOD
have the same meaning as for `completing-read'."
  (let ((minibuffer-local-completion-map
         (let ((map (copy-keymap minibuffer-local-completion-map)))
           (define-key map " " 'self-insert-command)
           map)))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))

(defun gpt-read-command ()
  "Read a GPT command from the user with history and completion."
  (let ((cmd (gpt-completing-read-space "Command: " gpt-command-history nil nil nil 'gpt-command-history)))
    (if (string-equal cmd "n/a")
        ""
      (string-trim cmd))))

(defun gpt-run-command (command input output-buffer)
  "Run GPT command on input and append output stream to output-buffer."
  (with-current-buffer output-buffer
    (insert (format "User: %s\n\nAssistant: " command))
    (goto-char (point-max))
    (font-lock-fontify-buffer)
    (let* ((prompt-file (gpt-create-prompt-file input command))
           (process (gpt-start-process prompt-file output-buffer))
           (timer (gpt-start-timer process)))
      (gpt-set-process-sentinel process timer prompt-file)
      (message "GPT: Running command..."))))

(defun gpt-dwim ()
  "Run user-provided GPT command on region and print output stream."
  (interactive)
  (let* ((initial-buffer (current-buffer))
         (command (gpt-read-command))
         (output-buffer (gpt-create-output-buffer initial-buffer)))
    (gpt-run-command command
                     (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       "")
                     output-buffer)
    (switch-to-buffer-other-window output-buffer)))

(defun gpt-follow-up ()
  "Run a follow-up GPT command on the output buffer and append the output stream."
  (interactive)
  (unless (eq major-mode 'gpt-mode)
    (user-error "Not in a gpt output buffer"))
  (let ((command (gpt-read-command)))
    (insert "\n\n")
    (gpt-run-command command (buffer-string) (current-buffer))))

(defun gpt-make-prompt (input command)
  "Create the prompt string from INPUT text and COMMAND."
  (cond ((and (string= input "") (string= command "")) "")
        ((string= input "") command)
        ((string= command "") input)
        (t (concat "\"\"\"\n" input "\n\"\"\"\n\n" command))))

(defun gpt-create-prompt-file (input command)
  "Create a temporary file containing the prompt string from INPUT text and COMMAND."
  (let ((temp-file (make-temp-file "gpt-prompt")))
    (with-temp-file temp-file
      (insert (gpt-make-prompt input command)))
    (message "GPT: Prompt written to %s" temp-file)
    temp-file))

(defun gpt-start-process (prompt-file output-buffer)
  "Start the GPT process with the given PROMPT-FILE and OUTPUT-BUFFER.
Use `gpt-script-path' as the executable and pass the other arguments as a list."
  (let ((process (start-process "gpt-process" output-buffer "python" gpt-script-path gpt-openai-key gpt-openai-engine gpt-openai-max-tokens gpt-openai-temperature prompt-file)))
    process))

(defun gpt-create-output-buffer (initial-buffer)
  "Create a temporary buffer to capture the output of the GPT process.
Use the `gpt-mode' for the output buffer."
  (let ((output-buffer (generate-new-buffer " *gpt*")))
    (with-current-buffer output-buffer
      (gpt-mode))
    output-buffer))

(defun gpt-start-timer (process)
  "Set timer to run every second and print message if PROCESS is still running."
  (run-with-timer 1 1
                  (lambda (timer-object)
                    (when (process-live-p timer-object)
                      (message "GPT: Running...")))
                  process))

(defun gpt-set-process-sentinel (process timer prompt-file)
  "Set a function to run when the PROCESS finishes or fails.

Cancel the timer, delete the prompt file, and print a message with the status.

PROCESS is the GPT process object.
TIMER is the timer object that cancels the process after a timeout.
PROMPT-FILE is the temporary file containing the prompt."
  (set-process-sentinel process
                        (lambda (proc status)
                          (when (memq (process-status proc) '(exit signal))
                            (cancel-timer timer)
                            (if (zerop (process-exit-status proc))
                                (progn
                                  (delete-file prompt-file)
                                  (message "GPT: Finished successfully."))
                              (message "GPT: Failed: %s" status))))))

(defface gpt-input-face
  '((t :inherit comint-highlight-prompt))
  "Face for the input of the GPT commands.")

(defface gpt-output-face
  '((t :inherit default))
  "Face for the output of the GPT commands.")

(defvar gpt-font-lock-keywords
  '(("^\\(User:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible gpt-prefix))
     (2 'gpt-input-face))
    ("^\\(Assistant:\\s-*\\)\\(.*\\)$"
     (1 '(face nil invisible gpt-prefix))
     (2 'gpt-output-face))))

(define-derived-mode gpt-mode text-mode "GPT"
  "A mode for displaying the output of GPT commands."
  (setq-local word-wrap t)
  (setq-local font-lock-defaults '(gpt-font-lock-keywords))
  (setq-local font-lock-extra-managed-props '(invisible))
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (add-to-invisibility-spec 'gpt-prefix))

(defun gpt-toggle-prefix ()
  "Toggle the visibility of the GPT prefixes."
  (interactive)
  (if (and (listp buffer-invisibility-spec)
           (memq 'gpt-prefix buffer-invisibility-spec))
        (remove-from-invisibility-spec 'gpt-prefix)
    (add-to-invisibility-spec 'gpt-prefix))
  (font-lock-fontify-buffer))

(define-key gpt-mode-map (kbd "C-c C-c") 'gpt-follow-up)
(define-key gpt-mode-map (kbd "C-c C-p") 'gpt-toggle-prefix)

(provide 'gpt)

;;; gpt.el ends here
