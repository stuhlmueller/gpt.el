;;; gpt-integration-test.el --- Integration tests for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Comprehensive integration tests for gpt.el that test real API access.
;; These tests require valid API keys to be set.
;;
;; Run with:
;;   emacs -Q -batch -l test/gpt-integration-test.el -f gpt-integration-run-tests
;;
;; Before running, set the API keys as environment variables:
;;   export OPENAI_API_KEY="your-key"
;;   export ANTHROPIC_API_KEY="your-key"
;;   export GOOGLE_API_KEY="your-key"

;;; Code:

(require 'ert)

;; Add parent directory to load-path
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "..")))

(require 'gpt)
(require 'gpt-core)
(require 'gpt-backend)
(require 'gpt-http)
(require 'gpt-openai)
(require 'gpt-anthropic)
(require 'gpt-google)

;;; ============================================================
;;; Test configuration
;;; ============================================================

(defvar gpt-integration-timeout 120
  "Timeout in seconds for integration tests.")

(defun gpt-integration-setup ()
  "Set up API keys from environment variables."
  (setq gpt-openai-key (getenv "OPENAI_API_KEY"))
  (setq gpt-anthropic-key (getenv "ANTHROPIC_API_KEY"))
  (setq gpt-google-key (getenv "GOOGLE_API_KEY")))

(defun gpt-integration-has-key-p (provider)
  "Return non-nil if API key for PROVIDER is available."
  (let ((key (cond ((eq provider 'openai) gpt-openai-key)
                   ((eq provider 'anthropic) gpt-anthropic-key)
                   ((eq provider 'google) gpt-google-key))))
    (and key (not (string-empty-p key)))))

;;; ============================================================
;;; Helper for synchronous testing
;;; ============================================================

(defvar gpt-integration--result nil)
(defvar gpt-integration--done nil)
(defvar gpt-integration--error nil)

(defun gpt-integration-wait-for-completion ()
  "Wait for an async operation to complete."
  (let ((start-time (current-time)))
    (while (and (not gpt-integration--done)
                (< (float-time (time-subtract (current-time) start-time))
                   gpt-integration-timeout))
      (accept-process-output nil 0.1))
    (unless gpt-integration--done
      (error "Integration test timed out after %d seconds" gpt-integration-timeout))))

(defun gpt-integration-reset ()
  "Reset integration test state."
  (setq gpt-integration--result nil
        gpt-integration--done nil
        gpt-integration--error nil))

;;; ============================================================
;;; OpenAI integration tests
;;; ============================================================

(ert-deftest gpt-integration-openai-simple ()
  "Test simple OpenAI API request with GPT-5 Nano."
  :tags '(integration openai)
  (skip-unless (gpt-integration-has-key-p 'openai))
  (gpt-integration-reset)
  (let* ((backend (gpt-openai-create gpt-openai-key))
         (messages '((:role "user" :content "Reply with exactly: HELLO")))
         (options '(:model "gpt-5.2" :max-tokens 100))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "HELLO" gpt-integration--result))))

(ert-deftest gpt-integration-openai-streaming ()
  "Test OpenAI streaming API request."
  :tags '(integration openai streaming)
  (skip-unless (gpt-integration-has-key-p 'openai))
  (skip-unless (gpt-http--curl-available-p))
  (gpt-integration-reset)
  (let* ((backend (gpt-openai-create gpt-openai-key))
         (messages '((:role "user" :content "Count from 1 to 5")))
         (options '(:model "gpt-5.2" :max-tokens 100))
         (data (gpt-backend-stream-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url))
         (chunks nil))
    (gpt-http-stream-request
     url headers data backend
     (lambda (content thinking)
       (when content
         (push content chunks)))
     (lambda (success error-msg)
       (if (not success)
           (setq gpt-integration--error error-msg))
       (setq gpt-integration--result (apply #'concat (nreverse chunks)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (> (length gpt-integration--result) 0))
    ;; Should contain some numbers
    (should (or (string-match-p "1" gpt-integration--result)
                (string-match-p "one" (downcase gpt-integration--result))))))

(ert-deftest gpt-integration-openai-system-message ()
  "Test OpenAI API with system message."
  :tags '(integration openai)
  (skip-unless (gpt-integration-has-key-p 'openai))
  (gpt-integration-reset)
  (let* ((backend (gpt-openai-create gpt-openai-key))
         (messages '((:role "system" :content "You always respond with exactly one word: PINEAPPLE")
                     (:role "user" :content "What is your favorite fruit?")))
         (options '(:model "gpt-5.2" :max-tokens 100))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "PINEAPPLE" gpt-integration--result))))

(ert-deftest gpt-integration-openai-multi-turn ()
  "Test OpenAI API with multi-turn conversation."
  :tags '(integration openai)
  (skip-unless (gpt-integration-has-key-p 'openai))
  (gpt-integration-reset)
  (let* ((backend (gpt-openai-create gpt-openai-key))
         (messages '((:role "user" :content "Remember this number: 42")
                     (:role "assistant" :content "I'll remember 42.")
                     (:role "user" :content "What number did I ask you to remember? Reply with just the number.")))
         (options '(:model "gpt-5.2" :max-tokens 100))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "42" gpt-integration--result))))

;;; ============================================================
;;; Anthropic integration tests
;;; ============================================================

(ert-deftest gpt-integration-anthropic-simple ()
  "Test simple Anthropic API request."
  :tags '(integration anthropic)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (gpt-integration-reset)
  (let* ((backend (gpt-anthropic-create gpt-anthropic-key
                                        :thinking-enabled nil))
         (messages '((:role "user" :content "Reply with exactly: HELLO")))
         (options '(:model "claude-3-5-haiku-latest" :max-tokens 10 :thinking-enabled nil))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "HELLO" gpt-integration--result))))

(ert-deftest gpt-integration-anthropic-thinking ()
  "Test Anthropic API request with thinking mode."
  :tags '(integration anthropic thinking)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (gpt-integration-reset)
  (let* ((backend (gpt-anthropic-create gpt-anthropic-key
                                        :thinking-enabled t
                                        :thinking-budget 10000
                                        :interleaved-thinking nil))
         (messages '((:role "user" :content "What is 2+2? Think step by step.")))
         (options '(:model "claude-3-7-sonnet-latest" :max-tokens 16000
                    :thinking-enabled t :thinking-budget 10000))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    ;; With thinking mode, result should be a plist or string
    (should (or (stringp gpt-integration--result)
                (plistp gpt-integration--result)))
    (let ((content (if (stringp gpt-integration--result)
                       gpt-integration--result
                     (plist-get gpt-integration--result :content))))
      (should (string-match-p "4" content)))))

(ert-deftest gpt-integration-anthropic-streaming ()
  "Test Anthropic streaming API request."
  :tags '(integration anthropic streaming)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (skip-unless (gpt-http--curl-available-p))
  (gpt-integration-reset)
  (let* ((backend (gpt-anthropic-create gpt-anthropic-key
                                        :thinking-enabled nil))
         (messages '((:role "user" :content "Count from 1 to 5")))
         (options '(:model "claude-3-5-haiku-latest" :max-tokens 50 :thinking-enabled nil))
         (data (gpt-backend-stream-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url))
         (chunks nil))
    (gpt-http-stream-request
     url headers data backend
     (lambda (content thinking)
       (when content
         (push content chunks)))
     (lambda (success error-msg)
       (if (not success)
           (setq gpt-integration--error error-msg))
       (setq gpt-integration--result (apply #'concat (nreverse chunks)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (> (length gpt-integration--result) 0))
    ;; Should contain some numbers
    (should (or (string-match-p "1" gpt-integration--result)
                (string-match-p "one" (downcase gpt-integration--result))))))

(ert-deftest gpt-integration-anthropic-system-message ()
  "Test Anthropic API with system message."
  :tags '(integration anthropic)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (gpt-integration-reset)
  (let* ((backend (gpt-anthropic-create gpt-anthropic-key
                                        :thinking-enabled nil))
         (messages '((:role "system" :content "You always respond with exactly one word: BANANA")
                     (:role "user" :content "What is your favorite fruit?")))
         (options '(:model "claude-3-5-haiku-latest" :max-tokens 10 :thinking-enabled nil))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "BANANA" gpt-integration--result))))

(ert-deftest gpt-integration-anthropic-multi-turn ()
  "Test Anthropic API with multi-turn conversation."
  :tags '(integration anthropic)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (gpt-integration-reset)
  (let* ((backend (gpt-anthropic-create gpt-anthropic-key
                                        :thinking-enabled nil))
         (messages '((:role "user" :content "Remember this number: 73")
                     (:role "assistant" :content "I'll remember 73.")
                     (:role "user" :content "What number did I ask you to remember? Reply with just the number.")))
         (options '(:model "claude-3-5-haiku-latest" :max-tokens 10 :thinking-enabled nil))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "73" gpt-integration--result))))

(ert-deftest gpt-integration-anthropic-web-search ()
  "Test Anthropic API with web search enabled."
  :tags '(integration anthropic web-search)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (gpt-integration-reset)
  (let* ((backend (gpt-anthropic-create gpt-anthropic-key
                                        :thinking-enabled nil
                                        :web-search t))
         (messages '((:role "user" :content "What is 2+2? Just say the number.")))
         (options '(:model "claude-3-5-haiku-latest" :max-tokens 50
                    :thinking-enabled nil :web-search t))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "4" gpt-integration--result))))

;;; ============================================================
;;; Google integration tests
;;; ============================================================

(ert-deftest gpt-integration-google-simple ()
  "Test simple Google API request."
  :tags '(integration google)
  (skip-unless (gpt-integration-has-key-p 'google))
  (gpt-integration-reset)
  (let* ((backend (gpt-google-create gpt-google-key))
         (messages '((:role "user" :content "Reply with exactly: HELLO")))
         (options '(:model "gemini-2.0-flash" :max-tokens 10))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (gpt-google--build-url backend "gemini-2.0-flash" nil)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "HELLO" gpt-integration--result))))

(ert-deftest gpt-integration-google-streaming ()
  "Test Google streaming API request."
  :tags '(integration google streaming)
  (skip-unless (gpt-integration-has-key-p 'google))
  (skip-unless (gpt-http--curl-available-p))
  (gpt-integration-reset)
  (let* ((backend (gpt-google-create gpt-google-key))
         (messages '((:role "user" :content "Count from 1 to 5")))
         (options '(:model "gemini-2.0-flash" :max-tokens 50))
         (data (gpt-backend-stream-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (gpt-google--build-url backend "gemini-2.0-flash" t))
         (chunks nil))
    (gpt-http-stream-request
     url headers data backend
     (lambda (content thinking)
       (when content
         (push content chunks)))
     (lambda (success error-msg)
       (if (not success)
           (setq gpt-integration--error error-msg))
       (setq gpt-integration--result (apply #'concat (nreverse chunks)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (> (length gpt-integration--result) 0))
    ;; Should contain some numbers
    (should (or (string-match-p "1" gpt-integration--result)
                (string-match-p "one" (downcase gpt-integration--result))))))

(ert-deftest gpt-integration-google-system-message ()
  "Test Google API with system instruction."
  :tags '(integration google)
  (skip-unless (gpt-integration-has-key-p 'google))
  (gpt-integration-reset)
  (let* ((backend (gpt-google-create gpt-google-key))
         (messages '((:role "system" :content "You always respond with exactly one word: MANGO")
                     (:role "user" :content "What is your favorite fruit?")))
         (options '(:model "gemini-2.0-flash" :max-tokens 10))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (gpt-google--build-url backend "gemini-2.0-flash" nil)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "MANGO" gpt-integration--result))))

(ert-deftest gpt-integration-google-multi-turn ()
  "Test Google API with multi-turn conversation."
  :tags '(integration google)
  (skip-unless (gpt-integration-has-key-p 'google))
  (gpt-integration-reset)
  (let* ((backend (gpt-google-create gpt-google-key))
         (messages '((:role "user" :content "Remember this number: 99")
                     (:role "assistant" :content "I'll remember 99.")
                     (:role "user" :content "What number did I ask you to remember? Reply with just the number.")))
         (options '(:model "gemini-2.0-flash" :max-tokens 10))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (gpt-google--build-url backend "gemini-2.0-flash" nil)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (string-match-p "99" gpt-integration--result))))

;;; ============================================================
;;; Model validation tests (all models in gpt-available-models)
;;; ============================================================

(ert-deftest gpt-integration-model-gpt-5.2 ()
  "Test GPT-5.2 model from gpt-available-models."
  :tags '(integration openai models)
  (skip-unless (gpt-integration-has-key-p 'openai))
  (gpt-integration-reset)
  (let* ((backend (gpt-openai-create gpt-openai-key))
         (messages '((:role "user" :content "Say only: OK")))
         (options '(:model "gpt-5.2" :max-tokens 50))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (> (length gpt-integration--result) 0))))

(ert-deftest gpt-integration-model-gpt-5.1 ()
  "Test GPT-5.1 model from gpt-available-models."
  :tags '(integration openai models)
  (skip-unless (gpt-integration-has-key-p 'openai))
  (gpt-integration-reset)
  (let* ((backend (gpt-openai-create gpt-openai-key))
         (messages '((:role "user" :content "Say only: OK")))
         (options '(:model "gpt-5.1" :max-tokens 50))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (> (length gpt-integration--result) 0))))

(ert-deftest gpt-integration-model-gpt-5-mini ()
  "Test GPT-5 Mini model from gpt-available-models."
  :tags '(integration openai models)
  (skip-unless (gpt-integration-has-key-p 'openai))
  (gpt-integration-reset)
  (let* ((backend (gpt-openai-create gpt-openai-key))
         (messages '((:role "user" :content "Say only: OK")))
         ;; GPT-5 Mini uses internal reasoning, needs more tokens
         (options '(:model "gpt-5-mini" :max-tokens 200))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))))

(ert-deftest gpt-integration-model-claude-opus-4-5 ()
  "Test Claude 4.5 Opus model from gpt-available-models."
  :tags '(integration anthropic models)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (gpt-integration-reset)
  (let* ((backend (gpt-anthropic-create gpt-anthropic-key
                                        :thinking-enabled nil))
         (messages '((:role "user" :content "Say only: OK")))
         (options '(:model "claude-opus-4-5" :max-tokens 50 :thinking-enabled nil))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (> (length gpt-integration--result) 0))))

(ert-deftest gpt-integration-model-claude-sonnet-4-5 ()
  "Test Claude 4.5 Sonnet model from gpt-available-models."
  :tags '(integration anthropic models)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (gpt-integration-reset)
  (let* ((backend (gpt-anthropic-create gpt-anthropic-key
                                        :thinking-enabled nil))
         (messages '((:role "user" :content "Say only: OK")))
         (options '(:model "claude-sonnet-4-5" :max-tokens 50 :thinking-enabled nil))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (oref backend url)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (> (length gpt-integration--result) 0))))

(ert-deftest gpt-integration-model-gemini-3-pro-preview ()
  "Test Gemini 3 Pro (Preview) model from gpt-available-models."
  :tags '(integration google models)
  (skip-unless (gpt-integration-has-key-p 'google))
  (gpt-integration-reset)
  (let* ((backend (gpt-google-create gpt-google-key))
         (messages '((:role "user" :content "Say only: OK")))
         (options '(:model "gemini-3-pro-preview" :max-tokens 1000))
         (data (gpt-backend-request-data backend messages options))
         (headers (gpt-backend-headers backend))
         (url (gpt-google--build-url backend "gemini-3-pro-preview" nil)))
    (gpt-http--url-request
     url headers data
     (lambda (response http-status error-msg)
       (if error-msg
           (setq gpt-integration--error error-msg)
         (setq gpt-integration--result
               (gpt-backend-parse-response backend response)))
       (setq gpt-integration--done t)))
    (gpt-integration-wait-for-completion)
    (should (null gpt-integration--error))
    (should (stringp gpt-integration--result))
    (should (> (length gpt-integration--result) 0))))

;;; ============================================================
;;; User path tests (using gpt-get-backend)
;;; ============================================================

(ert-deftest gpt-integration-user-path-openai ()
  "Test the full user path through gpt-get-backend for OpenAI."
  :tags '(integration openai user-path)
  (skip-unless (gpt-integration-has-key-p 'openai))
  (gpt-integration-reset)
  ;; Use the actual user path
  (let ((gpt-backends nil)  ; Clear cache
        (gpt-openai-key (getenv "OPENAI_API_KEY")))
    (let* ((backend (gpt-get-backend 'openai))
           (messages '((:role "user" :content "Say only: TEST")))
           (options '(:model "gpt-5.2" :max-tokens 10))
           (data (gpt-backend-request-data backend messages options))
           (headers (gpt-backend-headers backend))
           (url (oref backend url)))
      ;; Verify we got the right backend type
      (should (cl-typep backend 'gpt-openai-backend))
      (gpt-http--url-request
       url headers data
       (lambda (response http-status error-msg)
         (if error-msg
             (setq gpt-integration--error error-msg)
           (setq gpt-integration--result
                 (gpt-backend-parse-response backend response)))
         (setq gpt-integration--done t)))
      (gpt-integration-wait-for-completion)
      (should (null gpt-integration--error))
      (should (stringp gpt-integration--result)))))

(ert-deftest gpt-integration-user-path-anthropic ()
  "Test the full user path through gpt-get-backend for Anthropic."
  :tags '(integration anthropic user-path)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (gpt-integration-reset)
  ;; Use the actual user path
  (let ((gpt-backends nil)  ; Clear cache
        (gpt-anthropic-key (getenv "ANTHROPIC_API_KEY"))
        (gpt-thinking-enabled nil)
        (gpt-thinking-budget 10000)
        (gpt-interleaved-thinking nil)
        (gpt-web-search nil))
    (let* ((backend (gpt-get-backend 'anthropic))
           (messages '((:role "user" :content "Say only: TEST")))
           (options '(:model "claude-3-5-haiku-latest" :max-tokens 10
                      :thinking-enabled nil))
           (data (gpt-backend-request-data backend messages options))
           (headers (gpt-backend-headers backend))
           (url (oref backend url)))
      ;; Verify we got the right backend type
      (should (cl-typep backend 'gpt-anthropic-backend))
      (gpt-http--url-request
       url headers data
       (lambda (response http-status error-msg)
         (if error-msg
             (setq gpt-integration--error error-msg)
           (setq gpt-integration--result
                 (gpt-backend-parse-response backend response)))
         (setq gpt-integration--done t)))
      (gpt-integration-wait-for-completion)
      (should (null gpt-integration--error))
      (should (stringp gpt-integration--result)))))

(ert-deftest gpt-integration-user-path-google ()
  "Test the full user path through gpt-get-backend for Google."
  :tags '(integration google user-path)
  (skip-unless (gpt-integration-has-key-p 'google))
  (gpt-integration-reset)
  ;; Use the actual user path
  (let ((gpt-backends nil)  ; Clear cache
        (gpt-google-key (getenv "GOOGLE_API_KEY")))
    (let* ((backend (gpt-get-backend 'google))
           (messages '((:role "user" :content "Say only: TEST")))
           (options '(:model "gemini-2.0-flash" :max-tokens 10))
           (data (gpt-backend-request-data backend messages options))
           (headers (gpt-backend-headers backend))
           (url (gpt-google--build-url backend "gemini-2.0-flash" nil)))
      ;; Verify we got the right backend type
      (should (cl-typep backend 'gpt-google-backend))
      (gpt-http--url-request
       url headers data
       (lambda (response http-status error-msg)
         (if error-msg
             (setq gpt-integration--error error-msg)
           (setq gpt-integration--result
                 (gpt-backend-parse-response backend response)))
         (setq gpt-integration--done t)))
      (gpt-integration-wait-for-completion)
      (should (null gpt-integration--error))
      (should (stringp gpt-integration--result)))))

(ert-deftest gpt-integration-user-path-streaming-anthropic ()
  "Test the full streaming user path through gpt-get-backend for Anthropic."
  :tags '(integration anthropic user-path streaming)
  (skip-unless (gpt-integration-has-key-p 'anthropic))
  (skip-unless (gpt-http--curl-available-p))
  (gpt-integration-reset)
  ;; Use the actual user path
  (let ((gpt-backends nil)
        (gpt-anthropic-key (getenv "ANTHROPIC_API_KEY"))
        (gpt-thinking-enabled nil)
        (gpt-thinking-budget 10000)
        (gpt-interleaved-thinking nil)
        (gpt-web-search nil))
    (let* ((backend (gpt-get-backend 'anthropic))
           (messages '((:role "user" :content "Count from 1 to 3, separated by commas.")))
           (options '(:model "claude-3-5-haiku-latest" :max-tokens 20
                      :thinking-enabled nil))
           ;; Use stream-request-data - this is what failed for the user
           (data (gpt-backend-stream-request-data backend messages options))
           (headers (gpt-backend-headers backend))
           (url (oref backend url)))
      ;; Verify we got the right backend type
      (should (cl-typep backend 'gpt-anthropic-backend))
      ;; Verify stream flag is set
      (should (eq (plist-get data :stream) t))
      (gpt-http-stream-request
       url headers data backend
       (lambda (content _thinking)
         (when content
           (setq gpt-integration--result
                 (concat gpt-integration--result content))))
       (lambda (_success error-msg)
         (when error-msg
           (setq gpt-integration--error error-msg))
         (setq gpt-integration--done t)))
      (gpt-integration-wait-for-completion)
      (should (null gpt-integration--error))
      (should (stringp gpt-integration--result))
      (should (string-match-p "[123]" gpt-integration--result)))))

;;; ============================================================
;;; Test runner
;;; ============================================================

(defun gpt-integration-run-tests ()
  "Run all integration tests."
  (interactive)
  (gpt-integration-setup)
  (message "Running gpt.el integration tests...")
  (message "OpenAI key: %s" (if (gpt-integration-has-key-p 'openai) "set" "NOT SET"))
  (message "Anthropic key: %s" (if (gpt-integration-has-key-p 'anthropic) "set" "NOT SET"))
  (message "Google key: %s" (if (gpt-integration-has-key-p 'google) "set" "NOT SET"))
  (ert-run-tests-batch-and-exit '(tag integration)))

(defun gpt-integration-run-openai-tests ()
  "Run only OpenAI integration tests."
  (interactive)
  (gpt-integration-setup)
  (ert-run-tests-interactively '(tag openai)))

(defun gpt-integration-run-anthropic-tests ()
  "Run only Anthropic integration tests."
  (interactive)
  (gpt-integration-setup)
  (ert-run-tests-interactively '(tag anthropic)))

(defun gpt-integration-run-google-tests ()
  "Run only Google integration tests."
  (interactive)
  (gpt-integration-setup)
  (ert-run-tests-interactively '(tag google)))

(provide 'gpt-integration-test)
;;; gpt-integration-test.el ends here
