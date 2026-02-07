;;; gpt-http.el --- HTTP request layer for gpt.el -*- lexical-binding: t; package-lint-main-file: "gpt.el"; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file provides HTTP request functionality for gpt.el.
;; It supports both synchronous requests via url.el and
;; streaming requests via curl for real-time output.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'url-http)
(require 'gpt-backend)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

;;; Configuration

(defcustom gpt-use-curl t
  "Use curl for API requests when available.
Curl is required for streaming responses."
  :type 'boolean
  :group 'gpt)

(defcustom gpt-curl-program "curl"
  "Path to the curl executable."
  :type 'string
  :group 'gpt)

(defcustom gpt-http-max-retries 3
  "Maximum number of retries for transient failures."
  :type 'integer
  :group 'gpt)

(defcustom gpt-http-retry-delay 1.0
  "Base delay in seconds between retries.
Uses exponential backoff: delay * 2^attempt."
  :type 'number
  :group 'gpt)

;;; Error handling

(define-error 'gpt-http-error "GPT HTTP error")
(define-error 'gpt-api-error "GPT API error" 'gpt-http-error)
(define-error 'gpt-auth-error "GPT authentication error" 'gpt-api-error)
(define-error 'gpt-rate-limit-error "GPT rate limit error" 'gpt-api-error)

(defun gpt-http--retryable-status-p (status)
  "Return non-nil if HTTP STATUS is retryable."
  (memq status '(429 500 502 503 504)))

(defun gpt-http--parse-api-error (response http-status)
  "Parse API error from RESPONSE and HTTP-STATUS.
Returns a formatted error message string."
  (let* ((err (plist-get response :error))
         (err-type (or (plist-get err :type) "unknown"))
         (err-message (or (plist-get err :message)
                          (when (stringp err) err)
                          "Unknown error"))
         ;; Anthropic-specific
         (err-details (plist-get response :details))
         ;; OpenAI-specific
         (err-code (plist-get err :code)))
    (cond
     ;; Rate limit
     ((or (eq http-status 429)
          (string-match-p "rate" err-type))
      (format "Rate limit exceeded: %s" err-message))
     ;; Authentication
     ((or (eq http-status 401)
          (string-match-p "auth\\|invalid.*key" err-type))
      (format "Authentication failed: %s" err-message))
     ;; Invalid request
     ((eq http-status 400)
      (format "Invalid request: %s%s"
              err-message
              (if err-details (format " (%s)" err-details) "")))
     ;; Model not found
     ((eq http-status 404)
      (format "Model not found: %s" err-message))
     ;; Overloaded
     ((or (eq http-status 529)
          (string-match-p "overloaded" err-type))
      (format "API overloaded, please retry: %s" err-message))
     ;; Server error
     ((>= http-status 500)
      (format "Server error (%d): %s" http-status err-message))
     ;; Generic
     (t
      (format "API error (%d): %s%s"
              http-status
              err-message
              (if err-code (format " [%s]" err-code) ""))))))

;;; URL-based requests (non-streaming)

(defun gpt-http--url-request (url headers data callback &optional retry-count)
  "Make async HTTP POST request to URL with HEADERS and DATA.
CALLBACK is called with (RESPONSE HTTP-STATUS ERROR).
RETRY-COUNT is internal for retry logic."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (cons '("Content-Type" . "application/json") headers))
        (url-request-data (encode-coding-string
                           (gpt-backend--json-encode data) 'utf-8))
        (attempt (or retry-count 0)))
    (url-retrieve
     url
     (lambda (status)
       (let (response http-status error-msg)
         (if-let* ((err (plist-get status :error)))
             (setq error-msg (format "Network error: %s" err)
                   http-status 0)
           ;; Parse response
           (set-buffer-multibyte t)
           (goto-char (point-min))
           (when (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
             (setq http-status (string-to-number (match-string 1))))
           (when (and url-http-end-of-headers
                      (< url-http-end-of-headers (point-max)))
             (goto-char url-http-end-of-headers)
             (condition-case parse-err
                 (setq response (gpt-backend--json-read))
               (error
                (setq error-msg
                      (format "JSON parse error: %s. Response: %s"
                              (error-message-string parse-err)
                              (buffer-substring-no-properties
                               url-http-end-of-headers
                               (min (+ url-http-end-of-headers 500)
                                    (point-max)))))))))
         ;; Check for API errors in response body
         (when (and response (plist-get response :error))
           (setq error-msg (gpt-http--parse-api-error response http-status)))
         ;; Handle retries for transient errors
         (if (and (gpt-http--retryable-status-p http-status)
                  (< attempt gpt-http-max-retries))
             (let ((delay (* gpt-http-retry-delay (expt 2 attempt))))
               (kill-buffer (current-buffer))
               (message "GPT: Request failed with %d, retrying in %.1fs (attempt %d/%d)..."
                        http-status delay (1+ attempt) gpt-http-max-retries)
               (run-at-time delay nil
                            #'gpt-http--url-request url headers data callback (1+ attempt)))
           ;; No retry - call callback
           (kill-buffer (current-buffer))
           (funcall callback response http-status error-msg))))
     nil t nil)))

;;; Curl-based requests (streaming)

(defvar-local gpt-http--curl-process nil
  "The curl process for the current buffer.")

(defvar-local gpt-http--stream-buffer ""
  "Buffer for incomplete streaming data.")

(defvar-local gpt-http--stream-state nil
  "State for stream parsing.")

(defvar-local gpt-http--http-status nil
  "HTTP status code from curl response.")

(defun gpt-http--curl-available-p ()
  "Return non-nil if curl is available."
  (and gpt-use-curl
       (executable-find gpt-curl-program)))

(defun gpt-http--curl-args (url headers)
  "Build curl arguments for URL with HEADERS."
  (append
   (list "--silent"
         "--show-error"
         "--no-buffer"
         "-w" "\n__HTTP_STATUS__:%{http_code}"  ; Write status at end
         "-X" "POST"
         "-H" "Content-Type: application/json")
   ;; Add custom headers
   (mapcan (lambda (h)
             (list "-H" (format "%s: %s" (car h) (cdr h))))
           headers)
   (list url)))

(defun gpt-http-stream-request (url headers data backend on-chunk on-complete)
  "Make streaming HTTP POST request using curl.
URL is the API endpoint.
HEADERS is an alist of HTTP headers.
DATA is the request body (plist).
BACKEND is the gpt-backend instance for parsing.
ON-CHUNK is called with (CONTENT THINKING) for each chunk.
ON-COMPLETE is called with (SUCCESS ERROR-MSG) when done."
  (let* ((json-data (encode-coding-string
                     (gpt-backend--json-encode data) 'utf-8))
         (args (gpt-http--curl-args url headers))
         (process-buffer (generate-new-buffer " *gpt-curl*"))
         proc)
    (with-current-buffer process-buffer
      (setq-local gpt-http--stream-buffer "")
      (setq-local gpt-http--stream-state nil)
      (setq-local gpt-http--http-status nil))
    (setq proc
          (make-process
           :name "gpt-curl"
           :buffer process-buffer
           :command (append (list gpt-curl-program) args (list "-d" json-data))
           :coding 'utf-8
           :connection-type 'pipe
           :filter
           (lambda (proc output)
             (when (buffer-live-p (process-buffer proc))
               (with-current-buffer (process-buffer proc)
                 (setq gpt-http--stream-buffer
                       (concat gpt-http--stream-buffer output))
                 ;; Process complete lines
                 (gpt-http--process-stream-data
                  backend
                  (lambda (content thinking)
                    (funcall on-chunk content thinking))))))
           :sentinel
           (lambda (proc event)
             (let ((success nil)
                   (error-msg nil)
                   (http-status nil))
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   ;; Extract HTTP status from curl output
                   (when (string-match "__HTTP_STATUS__:\\([0-9]+\\)"
                                       gpt-http--stream-buffer)
                     (setq http-status (string-to-number (match-string 1 gpt-http--stream-buffer))))
                   ;; Determine success
                   (setq success (and (string-match-p "finished" event)
                                      (or (null http-status)
                                          (< http-status 400))))
                   ;; Check for process errors
                   (when (string-match-p "\\(exited\\|signal\\)" event)
                     (unless (string-match-p "finished" event)
                       (setq error-msg
                             (format "Process terminated: %s" (string-trim event)))))
                   ;; Check for API error in response
                   (when (string-match-p "\"error\"" gpt-http--stream-buffer)
                     (let ((buf gpt-http--stream-buffer)
                           (pos 0))
                       ;; Try parsing JSON from each { to find the error object
                       (while (and (not error-msg)
                                   (string-match "{" buf pos))
                         (setq pos (1+ (match-beginning 0)))
                         (condition-case nil
                             (let ((response (gpt-backend--json-read-from-string
                                              (substring buf (match-beginning 0)))))
                               (when (plist-get response :error)
                                 (setq success nil
                                       error-msg (gpt-http--parse-api-error
                                                  response (or http-status 0)))))
                           (error nil)))))
                   ;; If we have an HTTP error but no parsed message, provide generic one
                   (when (and (not success) (not error-msg) http-status (>= http-status 400))
                     (setq error-msg (format "HTTP error %d" http-status)))))
               ;; Call completion handler
               (funcall on-complete success error-msg)
               ;; Clean up process buffer
               (when (buffer-live-p (process-buffer proc))
                 (kill-buffer (process-buffer proc)))))))
    ;; Store process reference
    (with-current-buffer process-buffer
      (setq gpt-http--curl-process proc))
    proc))

(defun gpt-http--process-stream-data (backend on-data)
  "Process streaming data in current buffer using BACKEND.
ON-DATA is called with (CONTENT THINKING) for each parsed chunk."
  (let ((data gpt-http--stream-buffer)
        (pos 0))
    ;; Process complete lines
    (while (string-match "\n" data pos)
      (let ((line (substring data pos (match-beginning 0))))
        (setq pos (match-end 0))
        ;; Skip HTTP status line at end
        (unless (string-prefix-p "__HTTP_STATUS__" line)
          ;; Handle SSE data: lines
          (when (string-prefix-p "data: " line)
            (let ((json-str (substring line 6)))
              (unless (string= json-str "[DONE]")
                (condition-case nil
                    (let* ((result (gpt-backend-parse-stream-chunk
                                    backend json-str gpt-http--stream-state))
                           (content (plist-get result :content))
                           (thinking (plist-get result :thinking)))
                      (setq gpt-http--stream-state (plist-get result :state))
                      (when (or content thinking)
                        (funcall on-data content thinking)))
                  (error nil))))))))
    ;; Keep unprocessed data
    (setq gpt-http--stream-buffer (substring data pos))))

(defun gpt-http-abort (buffer)
  "Abort any running HTTP request in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and gpt-http--curl-process
                 (process-live-p gpt-http--curl-process))
        ;; Send SIGTERM first for clean shutdown
        (interrupt-process gpt-http--curl-process)
        ;; Give it a moment, then force kill if needed
        (run-at-time 0.5 nil
                     (lambda (proc)
                       (when (process-live-p proc)
                         (delete-process proc)))
                     gpt-http--curl-process)))))

(provide 'gpt-http)
;;; gpt-http.el ends here
