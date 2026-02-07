;;; gpt-google.el --- Google Gemini backend for gpt.el -*- lexical-binding: t; package-lint-main-file: "gpt.el"; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Keywords: google, gemini, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file implements the Google Gemini backend for gpt.el.

;;; Code:

(require 'gpt-backend)

;;; Google Gemini backend class

(defclass gpt-google-backend (gpt-backend)
  ((url
    :initform "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent")
   (stream-url
    :initform "https://generativelanguage.googleapis.com/v1beta/models/%s:streamGenerateContent")
   (name
    :initform "Google")
   (models
    :initform '("gemini-3-pro-preview" "gemini-2.5-pro-preview-06-05"
                "gemini-2.5-flash-preview-05-20" "gemini-2.0-flash"))
   (default-model
    :initform "gemini-3-pro-preview"))
  :documentation "Google Gemini API backend.")

;;; URL building

(defun gpt-google--build-url (backend model &optional stream)
  "Build API URL for BACKEND with MODEL.
If STREAM is non-nil, build streaming URL."
  (let ((url-template (if stream
                          (oref backend stream-url)
                        (oref backend url))))
    (concat (format url-template model)
            (when stream "?alt=sse"))))

;;; Implementation of generic methods

(cl-defmethod gpt-backend-headers ((backend gpt-google-backend))
  "Return HTTP headers for Google requests using BACKEND key."
  (list (cons "x-goog-api-key" (oref backend key))))

(defun gpt-google--convert-messages (messages)
  "Convert standard MESSAGES format to Google's format."
  (let ((contents nil)
        (system-instruction nil))
    (dolist (msg messages)
      (let ((role (plist-get msg :role))
            (content (plist-get msg :content)))
        (cond
         ((equal role "system")
          (setq system-instruction content))
         ((equal role "user")
          (push (list :role "user"
                      :parts (vector (list :text content)))
                contents))
         ((equal role "assistant")
          (push (list :role "model"
                      :parts (vector (list :text content)))
                contents)))))
    (list :contents (vconcat (nreverse contents))
          :system (when system-instruction
                    (list :parts (vector (list :text system-instruction)))))))

(cl-defmethod gpt-backend-request-data ((backend gpt-google-backend)
                                        messages options)
  "Build request data for Google API using BACKEND settings.
MESSAGES is a list of message plists.
OPTIONS is a plist with :model, :max-tokens, :temperature, etc."
  (let* ((model (or (plist-get options :model)
                    (oref backend default-model)))
         (max-tokens (plist-get options :max-tokens))
         (temperature (plist-get options :temperature))
         (converted (gpt-google--convert-messages messages))
         (data (list :contents (plist-get converted :contents))))
    ;; Add system instruction
    (when-let* ((system (plist-get converted :system)))
      (setq data (plist-put data :systemInstruction system)))
    ;; Add generation config
    (let ((gen-config nil))
      (when max-tokens
        (setq gen-config (plist-put gen-config :maxOutputTokens max-tokens)))
      (when temperature
        (setq gen-config (plist-put gen-config :temperature temperature)))
      (when gen-config
        (setq data (plist-put data :generationConfig gen-config))))
    ;; Store model in options for URL building
    (plist-put options :_model model)
    data))

(cl-defmethod gpt-backend-stream-request-data ((backend gpt-google-backend)
                                               messages options)
  "Build streaming request data for Google API from BACKEND.
MESSAGES and OPTIONS are as in `gpt-backend-request-data'."
  ;; Google streaming is via URL, not request body
  (gpt-backend-request-data backend messages options))

(cl-defmethod gpt-backend-parse-response ((_backend gpt-google-backend) response)
  "Parse Google API response and return content.
RESPONSE is the parsed JSON plist."
  (let* ((candidates (plist-get response :candidates))
         (candidate (and candidates (aref candidates 0)))
         (content (plist-get candidate :content))
         (parts (plist-get content :parts))
         (text-parts nil))
    (when parts
      (dotimes (i (length parts))
        (let ((part (aref parts i)))
          (when-let* ((text (plist-get part :text)))
            (push text text-parts)))))
    (mapconcat #'identity (nreverse text-parts) "")))

(cl-defmethod gpt-backend-parse-stream-chunk ((_backend gpt-google-backend)
                                              chunk state)
  "Parse a Google streaming chunk.
CHUNK is the raw JSON string.
STATE is the current parsing state."
  (condition-case nil
      (let* ((data (gpt-backend--json-read-from-string chunk))
             (candidates (plist-get data :candidates))
             (candidate (and candidates (aref candidates 0)))
             (content (plist-get candidate :content))
             (parts (plist-get content :parts))
             (finish-reason (plist-get candidate :finishReason))
             (text nil))
        (when parts
          (let ((part (aref parts 0)))
            (setq text (plist-get part :text))))
        (list :content text
              :done (and finish-reason t)
              :state state))
    (error (list :state state))))

;;; Constructor function

(defun gpt-google-create (api-key &rest args)
  "Create a Google backend instance with API-KEY.
ARGS are additional initialization arguments."
  (apply #'make-instance 'gpt-google-backend :key api-key args))

(provide 'gpt-google)
;;; gpt-google.el ends here
