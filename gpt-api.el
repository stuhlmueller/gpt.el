;;; gpt-api.el --- API functionality for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 2.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "24.4") (gpt-core "1.3"))

;;; Commentary:

;; This file contains API-related functions and process management for gpt.el.

;;; Code:

(require 'gpt-core)

(defun gpt-create-prompt-file (buffer)
  "Create a temporary file containing the prompt from BUFFER."
  (let ((temp-file (make-temp-file "gpt-prompt-")))
    (with-current-buffer buffer
      (write-region (point-min) (point-max) temp-file))
    temp-file))

(defun gpt-start-process (prompt-file buffer)
  "Start GPT process with PROMPT-FILE and output to BUFFER."
  (let* (;; Determine API key based on gpt-api-type
         (api-key (cond ((eq gpt-api-type 'openai) gpt-openai-key)
                        ((eq gpt-api-type 'anthropic) gpt-anthropic-key)
                        ((eq gpt-api-type 'google) gpt-google-key)
                        (t "NOT SET")))
         (process-environment
          (append
           (list
            (format "GPT_API_TYPE=%s" (symbol-name gpt-api-type))
            (format "GPT_MODEL=%s" gpt-model)
            (format "GPT_MAX_TOKENS=%s" gpt-max-tokens)
            (format "GPT_TEMPERATURE=%s" gpt-temperature)
            (format "OPENAI_API_KEY=%s" gpt-openai-key)
            (format "ANTHROPIC_API_KEY=%s" gpt-anthropic-key)
            (format "GOOGLE_API_KEY=%s" gpt-google-key))
           process-environment)))
    ;; Validate API key
    (when (string= api-key "NOT SET")
      (user-error "API key for %s is not set. Please configure %s"
                  (symbol-name gpt-api-type)
                  (cond ((eq gpt-api-type 'openai) "gpt-openai-key")
                        ((eq gpt-api-type 'anthropic) "gpt-anthropic-key")
                        ((eq gpt-api-type 'google) "gpt-google-key"))))
    ;; Validate script exists
    (unless (file-exists-p gpt-script-path)
      (user-error "GPT script not found at %s" gpt-script-path))
    ;; Create process with error handling
    (condition-case err
        (make-process
         :name "gpt"
         :buffer buffer
         :command (list gpt-python-path gpt-script-path 
                        api-key gpt-model gpt-max-tokens gpt-temperature 
                        (symbol-name gpt-api-type) prompt-file)
         :coding 'utf-8-unix
         :connection-type 'pipe)
      (error
       (gpt-message "Failed to start process: %s" (error-message-string err))
       nil))))

(defun gpt-start-timer (process)
  "Start a timer to check if PROCESS is still running."
  (run-with-timer
   0.1 0.1
   (lambda ()
     (unless (process-live-p process)
       (gpt-message "Command completed.")))))

(defun gpt-set-process-sentinel (process timer prompt-file)
  "Set up process sentinel for PROCESS with TIMER and PROMPT-FILE."
  (set-process-sentinel
   process
   (lambda (proc _msg)
     (when timer
       (cancel-timer timer))
     (when (file-exists-p prompt-file)
       (delete-file prompt-file))
     (when (eq (process-status proc) 'exit)
       (with-current-buffer (process-buffer proc)
         (save-excursion
           (goto-char (point-max))
           (unless (bolp)
             (insert "\n"))))))))

(defun gpt-message (format-string &rest args)
  "Display a message in the echo area using FORMAT-STRING and ARGS."
  (message (concat "GPT: " (apply #'format format-string args))))

(defun gpt-buffer-string (buffer)
  "Get BUFFER text as string."
  (with-current-buffer buffer
    (buffer-string)))

(defun gpt-run-buffer (buffer)
  "Run GPT command with BUFFER text as input.
Append output stream to output-buffer."
  (with-current-buffer buffer
    (goto-char (point-max))
    (font-lock-ensure)
    (let* ((prompt-file (gpt-create-prompt-file buffer))
           (process (gpt-start-process prompt-file buffer)))
      (if process
          (let ((timer (gpt-start-timer process)))
            (gpt-set-process-sentinel process timer prompt-file)
            (gpt-message "Running command...")
            (font-lock-ensure))
        ;; Process creation failed
        (when (file-exists-p prompt-file)
          (delete-file prompt-file))
        (gpt-message "Failed to start GPT process")))))

(provide 'gpt-api)
;;; gpt-api.el ends here 