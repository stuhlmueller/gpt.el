;;; gpt-api.el --- API functionality for gpt.el -*- lexical-binding: t; package-lint-main-file: "gpt.el"; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 3.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains API-related functions for gpt.el.
;; This is the pure Elisp version using url.el and curl for HTTP.

;;; Code:

(require 'gpt-core)
(require 'gpt-backend)
(require 'gpt-http)
(require 'eieio)

(declare-function gpt--start-spinner "gpt-mode" nil)
(declare-function gpt--stop-spinner "gpt-mode" nil)
(declare-function gpt-google--build-url "gpt-google" (backend model &optional stream))

;;; Request state tracking

(defvar-local gpt--request-process nil
  "The active request process for this buffer.")

(defvar-local gpt--output-marker nil
  "Marker for where to insert streamed output.")

;;; Message parsing

(defun gpt-parse-buffer-messages (buffer)
  "Parse BUFFER content into a list of message plists."
  (with-current-buffer buffer
    (gpt-backend--parse-messages
     (buffer-substring-no-properties (point-min) (point-max)))))

;;; Streaming output handling

(defun gpt--insert-thinking-start ()
  "Insert thinking block start marker."
  (insert "\n[Thinking...]\n"))

(defun gpt--insert-thinking-end ()
  "Insert thinking block end marker."
  (insert "\n[Thinking done.]\n\n"))

(defvar-local gpt--in-thinking-block nil
  "Non-nil if we're currently inside a thinking block.")

(defun gpt--insert-stream-output (buffer content thinking)
  "Insert streaming CONTENT and THINKING into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        ;; Handle thinking content
        (when thinking
          (save-excursion
            (goto-char (point-max))
            (unless gpt--in-thinking-block
              (gpt--insert-thinking-start)
              (setq gpt--in-thinking-block t))
            (insert thinking)))
        ;; Handle main content
        (when content
          (save-excursion
            (goto-char (point-max))
            ;; Close thinking block if open
            (when gpt--in-thinking-block
              (gpt--insert-thinking-end)
              (setq gpt--in-thinking-block nil))
            (insert content)))
        ;; Keep point at end if it was there
        (when at-end
          (goto-char (point-max)))))))

(defun gpt--finalize-stream (buffer success error-msg)
  "Finalize streaming request to BUFFER with SUCCESS and ERROR-MSG."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Close any open thinking block
      (when gpt--in-thinking-block
        (save-excursion
          (goto-char (point-max))
          (gpt--insert-thinking-end))
        (setq gpt--in-thinking-block nil))
      ;; Stop spinner
      (when (fboundp 'gpt--stop-spinner)
        (gpt--stop-spinner))
      ;; Ensure newline at end
      (save-excursion
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n")))
      ;; Report errors
      (if success
          (gpt-message "Request completed.")
        (gpt-message "Request failed: %s" (or error-msg "Unknown error"))))))

;;; API request functions

(defun gpt-run-buffer (buffer)
  "Run GPT request with BUFFER content as input.
Appends streaming output to the buffer.
Returns the request process when using curl, nil otherwise."
  (with-current-buffer buffer
    ;; Kill any existing process
    (when (and gpt--request-process
               (process-live-p gpt--request-process))
      (if (y-or-n-p "A GPT process is running here.  Kill it and start a new one? ")
          (delete-process gpt--request-process)
        (user-error "Aborted.  Existing GPT process is still running")))
    ;; Validate API key
    (gpt-validate-api-key)
    ;; Ensure we have a valid backend (not just non-nil, but correct type)
    (let* ((api-type (gpt--current-api-type))
           (messages (gpt-parse-buffer-messages buffer))
           (options (list :model gpt-model
                          :max-tokens gpt-max-tokens
                          :temperature (string-to-number gpt-temperature)))
           ;; Add provider-specific options
           (options (if (eq api-type 'anthropic)
                        (plist-put
                         (plist-put
                          (plist-put
                           (plist-put options :thinking-enabled gpt-thinking-enabled)
                           :thinking-budget gpt-thinking-budget)
                          :interleaved-thinking gpt-interleaved-thinking)
                         :web-search gpt-web-search)
                      options))
           (options (if (eq api-type 'openai)
                        (plist-put
                         (plist-put options :reasoning-effort gpt-openai-reasoning-effort)
                         :reasoning-summary gpt-openai-reasoning-summary)
                      options))
           ;; Build request
           (request-data (gpt-backend-stream-request-data
                          (or gpt-current-backend
                              (setq gpt-current-backend (gpt-get-backend api-type)))
                          messages options))
           (headers (gpt-backend-headers gpt-current-backend))
           (url (if (eq api-type 'google)
                    (progn
                      (require 'gpt-google)
                      (gpt-google--build-url gpt-current-backend gpt-model t))
                  (oref gpt-current-backend url))))
      (unless gpt-current-backend
        (user-error "Failed to create backend for %s" api-type))
      ;; Move to end of buffer for output
      (goto-char (point-max))
      (setq gpt--in-thinking-block nil)
      ;; Start spinner
      (when (fboundp 'gpt--start-spinner)
        (gpt--start-spinner))
      (gpt-message "Running request...")
      ;; Make streaming request
      (if (gpt-http--curl-available-p)
          (setq gpt--request-process
                (gpt-http-stream-request
                 url headers request-data gpt-current-backend
                 (lambda (content thinking)
                   (gpt--insert-stream-output buffer content thinking))
                 (lambda (success error-msg)
                   (gpt--finalize-stream buffer success error-msg))))
        ;; Fallback to non-streaming url-retrieve
        (gpt-http--url-request
         url headers request-data
         (lambda (response _http-status error-msg)
           (if error-msg
               (gpt--finalize-stream buffer nil error-msg)
             (let ((content (gpt-backend-parse-response gpt-current-backend response)))
               (if (stringp content)
                   (gpt--insert-stream-output buffer content nil)
                 ;; Handle response with thinking
                 (when-let* ((thinking (plist-get content :thinking)))
                   (gpt--insert-stream-output buffer nil thinking))
                 (gpt--insert-stream-output buffer (plist-get content :content) nil))
               (gpt--finalize-stream buffer t nil))))))
      ;; Return the process (nil if using url-retrieve fallback)
      gpt--request-process)))

(defun gpt-abort-request (&optional buffer)
  "Abort any active request in BUFFER (default current buffer)."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and gpt--request-process
                   (process-live-p gpt--request-process))
          (delete-process gpt--request-process)
          (setq gpt--request-process nil)
          (when (fboundp 'gpt--stop-spinner)
            (gpt--stop-spinner))
          (message "GPT request aborted"))))))

;;; Utility functions

(defun gpt-message (format-string &rest args)
  "Display a message in the echo area using FORMAT-STRING and ARGS."
  (message (concat "GPT: " (apply #'format format-string args))))

(defun gpt-buffer-string (buffer)
  "Get BUFFER text as string."
  (with-current-buffer buffer
    (buffer-string)))

(provide 'gpt-api)
;;; gpt-api.el ends here
