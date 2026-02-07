;;; gpt-anthropic.el --- Anthropic backend for gpt.el -*- lexical-binding: t; package-lint-main-file: "gpt.el"; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Keywords: anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file implements the Anthropic backend for gpt.el.
;; It supports Claude models with extended thinking and web search.

;;; Code:

(require 'gpt-backend)

;;; Anthropic backend class

(defclass gpt-anthropic-backend (gpt-backend)
  ((url
    :initform "https://api.anthropic.com/v1/messages")
   (name
    :initform "Anthropic")
   (models
    :initform '("claude-opus-4-5" "claude-sonnet-4-5" "claude-sonnet-4"
                "claude-3-5-sonnet-latest" "claude-3-5-haiku-latest"
                "claude-3-opus-latest"))
   (default-model
    :initform "claude-opus-4-5")
   (api-version
    :initarg :api-version
    :initform "2023-06-01"
    :type string
    :documentation "Anthropic API version.")
   ;; Thinking mode settings
   (thinking-enabled
    :initarg :thinking-enabled
    :initform t
    :type boolean
    :documentation "Enable extended thinking mode.")
   (thinking-budget
    :initarg :thinking-budget
    :initform 16000
    :type integer
    :documentation "Token budget for thinking.")
   (interleaved-thinking
    :initarg :interleaved-thinking
    :initform t
    :type boolean
    :documentation "Enable interleaved thinking with streaming.")
   (web-search
    :initarg :web-search
    :initform nil
    :type boolean
    :documentation "Enable web search tool."))
  :documentation "Anthropic Claude API backend.")

;;; Implementation of generic methods

(cl-defmethod gpt-backend-headers ((backend gpt-anthropic-backend))
  "Return HTTP headers for Anthropic requests using BACKEND settings."
  (let ((headers
         (list (cons "x-api-key" (oref backend key))
               (cons "anthropic-version" (oref backend api-version)))))
    ;; Add beta header for interleaved thinking
    (when (oref backend interleaved-thinking)
      (push (cons "anthropic-beta" "interleaved-thinking-2025-05-14") headers))
    headers))

(cl-defmethod gpt-backend-request-data ((backend gpt-anthropic-backend)
                                        messages options)
  "Build request data for Anthropic API using BACKEND settings.
MESSAGES is a list of message plists.
OPTIONS is a plist with :model, :max-tokens, :temperature, etc."
  (let* ((model (or (plist-get options :model)
                    (oref backend default-model)))
         (max-tokens (or (plist-get options :max-tokens) 8192))
         (temperature (plist-get options :temperature))
         (thinking-enabled (if (plist-member options :thinking-enabled)
                               (plist-get options :thinking-enabled)
                             (oref backend thinking-enabled)))
         (thinking-budget (or (plist-get options :thinking-budget)
                              (oref backend thinking-budget)))
         (web-search (if (plist-member options :web-search)
                         (plist-get options :web-search)
                       (oref backend web-search)))
         ;; Extract system message if first message is system
         (system-msg nil)
         (chat-messages messages))
    ;; Handle system message
    (when (and (car messages)
               (equal (plist-get (car messages) :role) "system"))
      (setq system-msg (plist-get (car messages) :content))
      (setq chat-messages (cdr messages)))
    ;; Build request
    (let ((data `(:model ,model
                  :max_tokens ,max-tokens
                  :messages ,(vconcat chat-messages))))
      ;; Add system message
      (when system-msg
        (setq data (plist-put data :system system-msg)))
      ;; Temperature must be 1.0 when thinking is enabled
      (when (and temperature (not thinking-enabled))
        (setq data (plist-put data :temperature temperature)))
      ;; Add thinking configuration
      (when thinking-enabled
        (setq data (plist-put data :thinking
                              `(:type "enabled"
                                :budget_tokens ,thinking-budget))))
      ;; Add web search tool
      (when web-search
        (setq data (plist-put data :tools
                              (vector '(:type "web_search_20250305"
                                        :name "web_search"
                                        :max_uses 5)))))
      data)))

(cl-defmethod gpt-backend-stream-request-data ((backend gpt-anthropic-backend)
                                               messages options)
  "Build streaming request data for Anthropic API from BACKEND.
MESSAGES and OPTIONS are as in `gpt-backend-request-data'."
  (let ((data (gpt-backend-request-data backend messages options)))
    (plist-put data :stream t)))

(cl-defmethod gpt-backend-parse-response ((_backend gpt-anthropic-backend) response)
  "Parse Anthropic API response and return content.
RESPONSE is the parsed JSON plist."
  (let ((content-blocks (plist-get response :content))
        (text-parts nil)
        (thinking-parts nil))
    ;; Process each content block
    (when content-blocks
      (dotimes (i (length content-blocks))
        (let* ((block (aref content-blocks i))
               (type (plist-get block :type)))
          (cond
           ((equal type "text")
            (push (plist-get block :text) text-parts))
           ((equal type "thinking")
            (push (plist-get block :thinking) thinking-parts))))))
    ;; Return combined content
    (let ((text (mapconcat #'identity (nreverse text-parts) ""))
          (thinking (mapconcat #'identity (nreverse thinking-parts) "")))
      (if (string-empty-p thinking)
          text
        (list :content text :thinking thinking)))))

(cl-defmethod gpt-backend-parse-stream-chunk ((_backend gpt-anthropic-backend)
                                              chunk state)
  "Parse an Anthropic streaming chunk.
CHUNK is the raw JSON string.
STATE is the current parsing state (tracks if we're in a thinking block)."
  (condition-case nil
      (let* ((data (gpt-backend--json-read-from-string chunk))
             (event-type (plist-get data :type))
             (result (list :state state)))
        (cond
         ;; Content block start
         ((equal event-type "content_block_start")
          (let* ((block (plist-get data :content_block))
                 (block-type (plist-get block :type)))
            (plist-put result :state
                       (if (equal block-type "thinking") 'thinking 'text))))
         ;; Content block delta
         ((equal event-type "content_block_delta")
          (let* ((delta (plist-get data :delta))
                 (delta-type (plist-get delta :type)))
            (cond
             ((equal delta-type "thinking_delta")
              (plist-put result :thinking (plist-get delta :thinking)))
             ((equal delta-type "text_delta")
              (plist-put result :content (plist-get delta :text))))))
         ;; Content block stop
         ((equal event-type "content_block_stop")
          (plist-put result :state nil))
         ;; Message complete
         ((equal event-type "message_stop")
          (plist-put result :done t)))
        result)
    (error (list :state state))))

;;; Constructor function

(defun gpt-anthropic-create (api-key &rest args)
  "Create an Anthropic backend instance with API-KEY.
ARGS are additional initialization arguments."
  (apply #'make-instance 'gpt-anthropic-backend :key api-key args))

(provide 'gpt-anthropic)
;;; gpt-anthropic.el ends here
