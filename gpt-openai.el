;;; gpt-openai.el --- OpenAI backend for gpt.el -*- lexical-binding: t; package-lint-main-file: "gpt.el"; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Keywords: openai, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file implements the OpenAI backend for gpt.el.
;; It supports both the Chat Completions API and the Responses API
;; for models with reasoning capabilities (GPT-5 family).

;;; Code:

(require 'gpt-backend)

;;; OpenAI backend class

(defclass gpt-openai-backend (gpt-backend)
  ((url
    :initform "https://api.openai.com/v1/chat/completions")
   (name
    :initform "OpenAI")
   (models
    :initform '("gpt-5.2" "gpt-5.1" "gpt-5-mini" "gpt-5-nano"
                "gpt-4.1" "gpt-4.1-mini" "gpt-4o" "gpt-4o-mini"))
   (default-model
    :initform "gpt-5.2")
   ;; OpenAI-specific slots
   (reasoning-effort
    :initarg :reasoning-effort
    :initform "medium"
    :type string
    :documentation "Reasoning effort: low, medium, or high.")
   (reasoning-summary
    :initarg :reasoning-summary
    :initform "detailed"
    :documentation "Reasoning summary: nil, auto, concise, or detailed."))
  :documentation "OpenAI API backend.")

;;; Implementation of generic methods

(cl-defmethod gpt-backend-headers ((backend gpt-openai-backend))
  "Return HTTP headers for OpenAI requests using BACKEND key."
  (list (cons "Authorization"
              (format "Bearer %s" (oref backend key)))))

(cl-defmethod gpt-backend-request-data ((backend gpt-openai-backend)
                                        messages options)
  "Build request data for OpenAI API using BACKEND settings.
MESSAGES is a list of message plists.
OPTIONS is a plist with :model, :max-tokens, :temperature, etc."
  (let* ((model (or (plist-get options :model)
                    (oref backend default-model)))
         (max-tokens (plist-get options :max-tokens))
         (temperature (plist-get options :temperature))
         ;; Only o1/o3 models support reasoning params, not gpt-5.x
         (is-reasoning (string-match-p "^o[0-9]" model))
         (data `(:model ,model
                 :messages ,(vconcat messages))))
    ;; Add max tokens
    (when max-tokens
      (setq data (plist-put data :max_completion_tokens max-tokens)))
    ;; Add temperature (not supported with reasoning models)
    (when (and temperature (not is-reasoning))
      (setq data (plist-put data :temperature temperature)))
    ;; Add reasoning parameters only for o1/o3 models
    (when is-reasoning
      (let ((effort (or (plist-get options :reasoning-effort)
                        (oref backend reasoning-effort)))
            (summary (or (plist-get options :reasoning-summary)
                         (oref backend reasoning-summary))))
        (setq data (plist-put data :reasoning
                              `(:effort ,effort
                                ,@(when summary
                                    (list :summary summary)))))))
    data))

(cl-defmethod gpt-backend-stream-request-data ((backend gpt-openai-backend)
                                               messages options)
  "Build streaming request data for OpenAI API from BACKEND.
MESSAGES and OPTIONS are as in `gpt-backend-request-data'."
  (let ((data (gpt-backend-request-data backend messages options)))
    (plist-put data :stream t)))

(cl-defmethod gpt-backend-parse-response ((_backend gpt-openai-backend) response)
  "Parse OpenAI API response and return content.
RESPONSE is the parsed JSON plist."
  (let* ((choices (plist-get response :choices))
         (choice (and choices (aref choices 0)))
         (message (plist-get choice :message))
         (content (plist-get message :content))
         (reasoning (plist-get message :reasoning_content)))
    ;; Return both content and reasoning if present
    (if reasoning
        (list :content content :reasoning reasoning)
      content)))

(cl-defmethod gpt-backend-parse-stream-chunk ((_backend gpt-openai-backend)
                                              chunk state)
  "Parse an OpenAI streaming chunk.
CHUNK is the raw JSON string.
STATE is the current parsing state."
  (condition-case nil
      (let* ((data (gpt-backend--json-read-from-string chunk))
             (choices (plist-get data :choices))
             (choice (and choices (aref choices 0)))
             (delta (plist-get choice :delta))
             (content (plist-get delta :content))
             (reasoning (plist-get delta :reasoning_content))
             (finish-reason (plist-get choice :finish_reason)))
        (list :content content
              :thinking reasoning
              :done (and finish-reason t)
              :state state))
    (error (list :state state))))

;;; Constructor function

(defun gpt-openai-create (api-key &rest args)
  "Create an OpenAI backend instance with API-KEY.
ARGS are additional initialization arguments."
  (apply #'make-instance 'gpt-openai-backend :key api-key args))

(provide 'gpt-openai)
;;; gpt-openai.el ends here
