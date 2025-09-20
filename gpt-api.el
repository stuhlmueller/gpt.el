;;; gpt-api.el --- API functionality for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 2.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "25.1"))

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
            (format "GOOGLE_API_KEY=%s" gpt-google-key)
            ;; OpenAI reasoning controls (for gpt-5 family)
            (format "GPT_OPENAI_REASONING_EFFORT=%s" gpt-openai-reasoning-effort)
            (format "GPT_OPENAI_REASONING_SUMMARY=%s" (or gpt-openai-reasoning-summary "")))
           process-environment))
         ;; Build command arguments
         (cmd-args (list gpt-python-path gpt-script-path
                         api-key gpt-model gpt-max-tokens gpt-temperature
                         (symbol-name gpt-api-type) prompt-file))
         ;; Create a hidden buffer to capture stderr to avoid default
         ;; sentinel inserting "Process ... finished" messages.
         (stderr-buffer (generate-new-buffer " *gpt-stderr*")))
    ;; Add thinking mode arguments for Anthropic
    (when (eq gpt-api-type 'anthropic)
      (when gpt-thinking-enabled
        (setq cmd-args (append cmd-args '("--thinking-enabled"))))
      (when gpt-thinking-enabled
        (setq cmd-args (append cmd-args (list "--thinking-budget" gpt-thinking-budget))))
      (when gpt-interleaved-thinking
        (setq cmd-args (append cmd-args '("--interleaved-thinking"))))
      (when gpt-web-search
        (setq cmd-args (append cmd-args '("--web-search")))))
    ;; Add web search argument if enabled and not already added for Anthropic
    (when (and gpt-web-search (not (eq gpt-api-type 'anthropic)))
      (setq cmd-args (append cmd-args '("--web-search"))))
    ;; Validate API key
    (when (string= api-key "NOT SET")
      (user-error "API key for %s is not set.  Please configure %s"
                  (symbol-name gpt-api-type)
                  (cond ((eq gpt-api-type 'openai) "gpt-openai-key")
                        ((eq gpt-api-type 'anthropic) "gpt-anthropic-key")
                        ((eq gpt-api-type 'google) "gpt-google-key"))))
    ;; Validate script exists
    (unless (file-exists-p gpt-script-path)
      (user-error "GPT script not found at %s" gpt-script-path))
    ;; Create process with error handling
    (condition-case err
        (let* ((proc (make-process
                      :name "gpt"
                      :buffer buffer
                      :command cmd-args
                      :coding 'utf-8-unix
                      :connection-type 'pipe
                      ;; Route stderr to hidden buffer; we'll mirror to BUFFER manually.
                      :stderr stderr-buffer))
               (stderr-proc (and (buffer-live-p stderr-buffer)
                                  (get-buffer-process stderr-buffer))))
          ;; Remember stderr buffer so we can clean it later.
          (process-put proc 'gpt-stderr-buffer stderr-buffer)
          ;; Mirror stderr chunks into the main output buffer without default sentinel noise.
          (when stderr-proc
            (set-process-filter stderr-proc
                                (lambda (_p chunk)
                                  (when (buffer-live-p buffer)
                                    (with-current-buffer buffer
                                      (let ((at-eob (= (point) (point-max))))
                                        (save-excursion
                                          (goto-char (point-max))
                                          (insert chunk))
                                        (when at-eob (goto-char (point-max))))))))
            ;; Prevent default "Process ... finished" insertion; clean hidden buffer on exit.
            (set-process-sentinel stderr-proc
                                  (lambda (p _msg)
                                    (let ((buf (process-buffer p)))
                                      (when (buffer-live-p buf)
                                        (kill-buffer buf))))))
          proc)
      (error
       (when (buffer-live-p stderr-buffer)
         (kill-buffer stderr-buffer))
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
     ;; Clean up stderr buffer if present
     (let ((stderr-buf (process-get proc 'gpt-stderr-buffer)))
       (when (buffer-live-p stderr-buf)
         (kill-buffer stderr-buf)))
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
