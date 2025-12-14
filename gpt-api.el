;;; gpt-api.el --- API functionality for gpt.el -*- lexical-binding: t; package-lint-main-file: "gpt.el"; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 2.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains API-related functions and process management for gpt.el.

;;; Code:

(require 'gpt-core)
(declare-function gpt--start-spinner "gpt-mode" nil)
(declare-function gpt--stop-spinner "gpt-mode" nil)

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
                        (t nil)))
         (process-environment
          (append
           (list
            ;; Non-secret config only - API keys passed via stdin for security
            (format "GPT_API_TYPE=%s" (symbol-name gpt-api-type))
            (format "GPT_MODEL=%s" gpt-model)
            (format "GPT_MAX_TOKENS=%s" gpt-max-tokens)
            (format "GPT_TEMPERATURE=%s" gpt-temperature)
            ;; OpenAI reasoning controls (for gpt-5 family)
            (format "GPT_OPENAI_REASONING_EFFORT=%s" gpt-openai-reasoning-effort)
            (format "GPT_OPENAI_REASONING_SUMMARY=%s" (or gpt-openai-reasoning-summary "")))
           process-environment))
         ;; Build command arguments (api_key removed - passed via stdin)
         (cmd-args (list gpt-python-path gpt-script-path
                         gpt-model gpt-max-tokens gpt-temperature
                         (symbol-name gpt-api-type) prompt-file))
         ;; Create a hidden buffer to capture stderr to avoid default
         ;; sentinel inserting "Process ... finished" messages.
         (stderr-buffer (generate-new-buffer " *gpt-stderr*")))
    ;; Validate API key (handles nil, empty, etc.)
    (gpt-validate-api-key)
    ;; Add thinking mode arguments for Anthropic
    (when (eq gpt-api-type 'anthropic)
      (when gpt-thinking-enabled
        (setq cmd-args (append cmd-args
                               (list "--thinking-enabled"
                                     "--thinking-budget" gpt-thinking-budget))))
      (when gpt-interleaved-thinking
        (setq cmd-args (append cmd-args '("--interleaved-thinking"))))
      (when gpt-web-search
        (setq cmd-args (append cmd-args '("--web-search")))))
    ;; Add web search argument if enabled and not already added for Anthropic
    (when (and gpt-web-search (not (eq gpt-api-type 'anthropic)))
      (setq cmd-args (append cmd-args '("--web-search"))))
    ;; Validate thinking mode constraints for Anthropic
    (when (and (eq gpt-api-type 'anthropic) gpt-thinking-enabled)
      (let ((budget (string-to-number gpt-thinking-budget))
            (max-tok (string-to-number gpt-max-tokens)))
        (when (>= budget max-tok)
          (user-error "Thinking budget (%s) must be less than max_tokens (%s)"
                      gpt-thinking-budget gpt-max-tokens))))
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
          ;; Send API key via stdin (more secure than env vars or command line)
          (process-send-string proc (concat api-key "\n"))
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
      (file-error
       (when (buffer-live-p stderr-buffer)
         (kill-buffer stderr-buffer))
       (gpt-message "Failed to start process (file error): %s" (error-message-string err))
       nil)
      (error
       (when (buffer-live-p stderr-buffer)
         (kill-buffer stderr-buffer))
       (gpt-message "Failed to start process: %s" (error-message-string err))
       nil))))

(defun gpt-start-timer (process)
  "Start a timer to check if PROCESS is still running."
  (let (timer)
    (setq timer
          (run-with-timer
           0.1 0.1
           (lambda ()
             (unless (process-live-p process)
               (when timer
                 (cancel-timer timer))
               (gpt-message "Command completed.")))))
    timer))

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
         ;; Stop spinner if available
         (when (fboundp 'gpt--stop-spinner)
           (gpt--stop-spinner))
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
    ;; If a process is already running for this buffer, prompt to stop it first.
    (let ((existing (get-buffer-process (current-buffer))))
      (when (and existing (process-live-p existing))
        (if (y-or-n-p "A GPT process is running here. Kill it and start a new one? ")
            (progn (delete-process existing)
                   ;; Give Emacs a tick to run process sentinel cleanup
                   (sit-for 0.05))
          (user-error "Aborted. Existing GPT process is still running."))))
    (goto-char (point-max))
    (font-lock-ensure)
    (let* ((prompt-file (gpt-create-prompt-file buffer))
           (process (gpt-start-process prompt-file buffer)))
      (if process
          (let ((timer (gpt-start-timer process)))
            (gpt-set-process-sentinel process timer prompt-file)
            (gpt-message "Running command...")
            ;; Start mode-line spinner if available
            (when (fboundp 'gpt--start-spinner)
              (with-current-buffer buffer (gpt--start-spinner)))
            (font-lock-ensure))
        ;; Process creation failed
        (when (file-exists-p prompt-file)
          (delete-file prompt-file))
          (gpt-message "Failed to start GPT process")))))

(provide 'gpt-api)
;;; gpt-api.el ends here
