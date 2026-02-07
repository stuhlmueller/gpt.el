;;; gpt-test.el --- Unit tests for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Comprehensive unit tests for gpt.el that don't require API access.
;; Run with: emacs -Q -batch -l test/gpt-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'json)

;; Add parent directory to load-path
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "..")))

(require 'gpt-backend)
(require 'gpt-openai)
(require 'gpt-anthropic)
(require 'gpt-google)
(require 'gpt-http)
(require 'gpt-core)

;;; ============================================================
;;; Message parsing tests
;;; ============================================================

(ert-deftest gpt-test-parse-messages-empty ()
  "Test parsing empty input."
  (should (null (gpt-backend--parse-messages ""))))

(ert-deftest gpt-test-parse-messages-single-user ()
  "Test parsing single user message."
  (let ((messages (gpt-backend--parse-messages "User: Hello world")))
    (should (= (length messages) 1))
    (should (equal (plist-get (car messages) :role) "user"))
    (should (equal (plist-get (car messages) :content) "Hello world"))))

(ert-deftest gpt-test-parse-messages-simple ()
  "Test parsing simple User/Assistant format."
  (let ((messages (gpt-backend--parse-messages
                   "User: Hello\n\nAssistant: Hi there!")))
    (should (= (length messages) 2))
    (should (equal (plist-get (car messages) :role) "user"))
    (should (equal (plist-get (car messages) :content) "Hello"))
    (should (equal (plist-get (cadr messages) :role) "assistant"))
    (should (equal (plist-get (cadr messages) :content) "Hi there!"))))

(ert-deftest gpt-test-parse-messages-human-model ()
  "Test parsing Human/Model format (alternative)."
  (let ((messages (gpt-backend--parse-messages
                   "Human: What is 2+2?\n\nModel: 4")))
    (should (= (length messages) 2))
    (should (equal (plist-get (car messages) :role) "user"))
    (should (equal (plist-get (cadr messages) :role) "assistant"))))

(ert-deftest gpt-test-parse-messages-multiline ()
  "Test parsing messages with multiline content."
  (let ((messages (gpt-backend--parse-messages
                   "User: First line\nSecond line\nThird line\n\nAssistant: Response")))
    (should (= (length messages) 2))
    (should (string-match-p "First line" (plist-get (car messages) :content)))
    (should (string-match-p "Second line" (plist-get (car messages) :content)))
    (should (string-match-p "Third line" (plist-get (car messages) :content)))))

(ert-deftest gpt-test-parse-messages-multi-turn ()
  "Test parsing multi-turn conversation."
  (let ((messages (gpt-backend--parse-messages
                   "User: First question\n\nAssistant: First answer\n\nUser: Second question\n\nAssistant: Second answer")))
    (should (= (length messages) 4))
    (should (equal (plist-get (nth 0 messages) :role) "user"))
    (should (equal (plist-get (nth 1 messages) :role) "assistant"))
    (should (equal (plist-get (nth 2 messages) :role) "user"))
    (should (equal (plist-get (nth 3 messages) :role) "assistant"))))

(ert-deftest gpt-test-parse-messages-whitespace ()
  "Test parsing handles whitespace correctly."
  (let ((messages (gpt-backend--parse-messages
                   "User:   Padded message   \n\nAssistant: Response")))
    (should (= (length messages) 2))
    ;; Content should be trimmed
    (should (equal (plist-get (car messages) :content) "Padded message"))))

;;; ============================================================
;;; JSON encoding/parsing tests
;;; ============================================================

(ert-deftest gpt-test-json-encode-basic ()
  "Test basic JSON encoding."
  (let ((json (gpt-backend--json-encode '(:model "gpt-4" :temperature 0.5))))
    (should (string-match-p "\"model\"" json))
    (should (string-match-p "\"gpt-4\"" json))
    (should (string-match-p "\"temperature\"" json))))

(ert-deftest gpt-test-json-encode-nested ()
  "Test nested JSON encoding."
  (let ((json (gpt-backend--json-encode
               '(:thinking (:type "enabled" :budget_tokens 10000)))))
    (should (string-match-p "\"thinking\"" json))
    (should (string-match-p "\"type\"" json))
    (should (string-match-p "\"enabled\"" json))
    (should (string-match-p "\"budget_tokens\"" json))))

(ert-deftest gpt-test-json-encode-array ()
  "Test array JSON encoding."
  (let ((json (gpt-backend--json-encode
               '(:messages [(:role "user" :content "Hello")]))))
    (should (string-match-p "\\[" json))
    (should (string-match-p "\"role\"" json))))

(ert-deftest gpt-test-json-read-from-string ()
  "Test JSON parsing from string."
  (let ((data (gpt-backend--json-read-from-string "{\"name\": \"test\", \"value\": 42}")))
    (should (equal (plist-get data :name) "test"))
    (should (equal (plist-get data :value) 42))))

(ert-deftest gpt-test-json-read-nested ()
  "Test nested JSON parsing."
  (let ((data (gpt-backend--json-read-from-string
               "{\"outer\": {\"inner\": \"value\"}}")))
    (should (equal (plist-get (plist-get data :outer) :inner) "value"))))

(ert-deftest gpt-test-json-read-array ()
  "Test array JSON parsing."
  (let ((data (gpt-backend--json-read-from-string
               "{\"items\": [1, 2, 3]}")))
    (should (vectorp (plist-get data :items)))
    (should (= (length (plist-get data :items)) 3))))

;;; ============================================================
;;; OpenAI backend tests
;;; ============================================================

(ert-deftest gpt-test-openai-create ()
  "Test creating OpenAI backend."
  (let ((backend (gpt-openai-create "test-key")))
    (should (equal (oref backend name) "OpenAI"))
    (should (equal (oref backend key) "test-key"))
    (should (member "gpt-5.2" (oref backend models)))
    (should (equal (oref backend default-model) "gpt-5.2"))))

(ert-deftest gpt-test-openai-create-with-options ()
  "Test creating OpenAI backend with custom options."
  (let ((backend (gpt-openai-create "test-key"
                                    :reasoning-effort "high"
                                    :reasoning-summary "concise")))
    (should (equal (oref backend reasoning-effort) "high"))
    (should (equal (oref backend reasoning-summary) "concise"))))

(ert-deftest gpt-test-openai-headers ()
  "Test OpenAI header generation."
  (let* ((backend (gpt-openai-create "test-api-key"))
         (headers (gpt-backend-headers backend)))
    (should (assoc "Authorization" headers))
    (should (string-match-p "Bearer test-api-key"
                            (cdr (assoc "Authorization" headers))))))

(ert-deftest gpt-test-openai-request-data-basic ()
  "Test OpenAI basic request data building."
  (let* ((backend (gpt-openai-create "test-key"))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "gpt-4o" :max-tokens 100 :temperature 0.5))
         (data (gpt-backend-request-data backend messages options)))
    (should (equal (plist-get data :model) "gpt-4o"))
    (should (equal (plist-get data :max_completion_tokens) 100))
    (should (equal (plist-get data :temperature) 0.5))
    (should (vectorp (plist-get data :messages)))))

(ert-deftest gpt-test-openai-request-data-reasoning ()
  "Test OpenAI reasoning mode request data (o1/o3 models)."
  (let* ((backend (gpt-openai-create "test-key"))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "o3-mini" :max-tokens 100))
         (data (gpt-backend-request-data backend messages options)))
    (should (equal (plist-get data :model) "o3-mini"))
    ;; o1/o3 should have reasoning parameters
    (let ((reasoning (plist-get data :reasoning)))
      (should reasoning)
      (should (equal (plist-get reasoning :effort) "medium"))
      (should (equal (plist-get reasoning :summary) "detailed")))
    ;; Temperature should NOT be set for reasoning models
    (should (null (plist-get data :temperature)))))

(ert-deftest gpt-test-openai-request-data-reasoning-custom ()
  "Test OpenAI reasoning with custom effort/summary."
  (let* ((backend (gpt-openai-create "test-key"
                                     :reasoning-effort "high"
                                     :reasoning-summary "concise"))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "o1" :max-tokens 100))
         (data (gpt-backend-request-data backend messages options)))
    (let ((reasoning (plist-get data :reasoning)))
      (should (equal (plist-get reasoning :effort) "high"))
      (should (equal (plist-get reasoning :summary) "concise")))))

(ert-deftest gpt-test-openai-request-data-gpt5-no-reasoning ()
  "Test that GPT-5 models do NOT get reasoning parameters."
  (let* ((backend (gpt-openai-create "test-key"))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "gpt-5.2" :max-tokens 100 :temperature 0.7))
         (data (gpt-backend-request-data backend messages options)))
    (should (equal (plist-get data :model) "gpt-5.2"))
    ;; GPT-5 should NOT have reasoning parameters
    (should (null (plist-get data :reasoning)))
    ;; Temperature SHOULD be set for non-reasoning models
    (should (equal (plist-get data :temperature) 0.7))))

(ert-deftest gpt-test-openai-stream-request-data ()
  "Test OpenAI streaming request data."
  (let* ((backend (gpt-openai-create "test-key"))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "gpt-4o" :max-tokens 100))
         (data (gpt-backend-stream-request-data backend messages options)))
    (should (eq (plist-get data :stream) t))))

(ert-deftest gpt-test-openai-parse-response ()
  "Test OpenAI response parsing."
  (let* ((backend (gpt-openai-create "test-key"))
         (response '(:choices [(:message (:content "Hello world"))]))
         (content (gpt-backend-parse-response backend response)))
    (should (equal content "Hello world"))))

(ert-deftest gpt-test-openai-parse-response-with-reasoning ()
  "Test OpenAI response parsing with reasoning content."
  (let* ((backend (gpt-openai-create "test-key"))
         (response '(:choices [(:message (:content "The answer is 4"
                                          :reasoning_content "Let me think... 2+2=4"))]))
         (content (gpt-backend-parse-response backend response)))
    (should (listp content))
    (should (equal (plist-get content :content) "The answer is 4"))
    (should (equal (plist-get content :reasoning) "Let me think... 2+2=4"))))

(ert-deftest gpt-test-openai-parse-stream-chunk ()
  "Test OpenAI streaming chunk parsing."
  (let* ((backend (gpt-openai-create "test-key"))
         (chunk "{\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}")
         (result (gpt-backend-parse-stream-chunk backend chunk nil)))
    (should (equal (plist-get result :content) "Hello"))))

(ert-deftest gpt-test-openai-parse-stream-chunk-reasoning ()
  "Test OpenAI streaming chunk parsing with reasoning."
  (let* ((backend (gpt-openai-create "test-key"))
         (chunk "{\"choices\":[{\"delta\":{\"reasoning_content\":\"Thinking...\"}}]}")
         (result (gpt-backend-parse-stream-chunk backend chunk nil)))
    (should (equal (plist-get result :thinking) "Thinking..."))))

(ert-deftest gpt-test-openai-parse-stream-chunk-done ()
  "Test OpenAI streaming chunk parsing finish."
  (let* ((backend (gpt-openai-create "test-key"))
         (chunk "{\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}")
         (result (gpt-backend-parse-stream-chunk backend chunk nil)))
    (should (plist-get result :done))))

;;; ============================================================
;;; Anthropic backend tests
;;; ============================================================

(ert-deftest gpt-test-anthropic-create ()
  "Test creating Anthropic backend."
  (let ((backend (gpt-anthropic-create "test-key")))
    (should (equal (oref backend name) "Anthropic"))
    (should (equal (oref backend key) "test-key"))
    (should (member "claude-opus-4-5" (oref backend models)))))

(ert-deftest gpt-test-anthropic-create-with-options ()
  "Test creating Anthropic backend with custom options."
  (let ((backend (gpt-anthropic-create "test-key"
                                       :thinking-enabled nil
                                       :thinking-budget 5000
                                       :web-search t)))
    (should (eq (oref backend thinking-enabled) nil))
    (should (equal (oref backend thinking-budget) 5000))
    (should (eq (oref backend web-search) t))))

(ert-deftest gpt-test-anthropic-headers-basic ()
  "Test Anthropic basic header generation."
  (let* ((backend (gpt-anthropic-create "test-api-key"
                                        :interleaved-thinking nil))
         (headers (gpt-backend-headers backend)))
    (should (assoc "x-api-key" headers))
    (should (equal (cdr (assoc "x-api-key" headers)) "test-api-key"))
    (should (assoc "anthropic-version" headers))
    ;; No beta header when interleaved thinking is disabled
    (should (null (assoc "anthropic-beta" headers)))))

(ert-deftest gpt-test-anthropic-headers-interleaved ()
  "Test Anthropic header generation with interleaved thinking."
  (let* ((backend (gpt-anthropic-create "test-api-key"
                                        :interleaved-thinking t))
         (headers (gpt-backend-headers backend)))
    (should (assoc "anthropic-beta" headers))
    (should (string-match-p "interleaved-thinking"
                            (cdr (assoc "anthropic-beta" headers))))))

(ert-deftest gpt-test-anthropic-request-data-basic ()
  "Test Anthropic basic request data building."
  (let* ((backend (gpt-anthropic-create "test-key" :thinking-enabled nil))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "claude-3" :max-tokens 100 :thinking-enabled nil))
         (data (gpt-backend-request-data backend messages options)))
    (should (equal (plist-get data :model) "claude-3"))
    (should (equal (plist-get data :max_tokens) 100))
    (should (null (plist-get data :thinking)))))

(ert-deftest gpt-test-anthropic-request-data-thinking ()
  "Test Anthropic request data with thinking enabled."
  (let* ((backend (gpt-anthropic-create "test-key" :thinking-enabled t))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "claude-3" :max-tokens 100))
         (data (gpt-backend-request-data backend messages options)))
    (should (plist-get data :thinking))
    (let ((thinking (plist-get data :thinking)))
      (should (equal (plist-get thinking :type) "enabled"))
      (should (numberp (plist-get thinking :budget_tokens))))))

(ert-deftest gpt-test-anthropic-request-data-web-search ()
  "Test Anthropic request data with web search enabled."
  (let* ((backend (gpt-anthropic-create "test-key"
                                        :thinking-enabled nil
                                        :web-search t))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "claude-3" :max-tokens 100 :thinking-enabled nil))
         (data (gpt-backend-request-data backend messages options)))
    (should (plist-get data :tools))
    (let* ((tools (plist-get data :tools))
           (tool (aref tools 0)))
      (should (equal (plist-get tool :type) "web_search_20250305")))))

(ert-deftest gpt-test-anthropic-request-data-system-message ()
  "Test Anthropic request data with system message."
  (let* ((backend (gpt-anthropic-create "test-key" :thinking-enabled nil))
         (messages '((:role "system" :content "You are helpful")
                     (:role "user" :content "Hello")))
         (options '(:model "claude-3" :max-tokens 100 :thinking-enabled nil))
         (data (gpt-backend-request-data backend messages options)))
    (should (equal (plist-get data :system) "You are helpful"))
    ;; Messages should not include system message
    (let ((msgs (plist-get data :messages)))
      (should (= (length msgs) 1))
      (should (equal (plist-get (aref msgs 0) :role) "user")))))

(ert-deftest gpt-test-anthropic-stream-request-data ()
  "Test Anthropic streaming request data."
  (let* ((backend (gpt-anthropic-create "test-key"))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "claude-3" :max-tokens 100))
         (data (gpt-backend-stream-request-data backend messages options)))
    (should (eq (plist-get data :stream) t))))

(ert-deftest gpt-test-anthropic-parse-response ()
  "Test Anthropic response parsing."
  (let* ((backend (gpt-anthropic-create "test-key"))
         (response '(:content [(:type "text" :text "Hello world")]))
         (content (gpt-backend-parse-response backend response)))
    (should (equal content "Hello world"))))

(ert-deftest gpt-test-anthropic-parse-response-multi-block ()
  "Test Anthropic response parsing with multiple text blocks."
  (let* ((backend (gpt-anthropic-create "test-key"))
         (response '(:content [(:type "text" :text "First ")
                               (:type "text" :text "Second")]))
         (content (gpt-backend-parse-response backend response)))
    (should (equal content "First Second"))))

(ert-deftest gpt-test-anthropic-parse-response-with-thinking ()
  "Test Anthropic response parsing with thinking blocks."
  (let* ((backend (gpt-anthropic-create "test-key"))
         (response '(:content [(:type "thinking" :thinking "Let me think...")
                               (:type "text" :text "The answer is 42")]))
         (content (gpt-backend-parse-response backend response)))
    (should (listp content))
    (should (equal (plist-get content :content) "The answer is 42"))
    (should (equal (plist-get content :thinking) "Let me think..."))))

(ert-deftest gpt-test-anthropic-parse-stream-chunk-text ()
  "Test Anthropic streaming chunk parsing for text."
  (let* ((backend (gpt-anthropic-create "test-key"))
         (chunk "{\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}")
         (result (gpt-backend-parse-stream-chunk backend chunk nil)))
    (should (equal (plist-get result :content) "Hello"))))

(ert-deftest gpt-test-anthropic-parse-stream-chunk-thinking ()
  "Test Anthropic streaming chunk parsing for thinking."
  (let* ((backend (gpt-anthropic-create "test-key"))
         (chunk "{\"type\":\"content_block_delta\",\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"Let me see...\"}}")
         (result (gpt-backend-parse-stream-chunk backend chunk nil)))
    (should (equal (plist-get result :thinking) "Let me see..."))))

(ert-deftest gpt-test-anthropic-parse-stream-chunk-block-start ()
  "Test Anthropic streaming content_block_start parsing."
  (let* ((backend (gpt-anthropic-create "test-key"))
         (chunk "{\"type\":\"content_block_start\",\"content_block\":{\"type\":\"thinking\"}}")
         (result (gpt-backend-parse-stream-chunk backend chunk nil)))
    (should (eq (plist-get result :state) 'thinking))))

(ert-deftest gpt-test-anthropic-parse-stream-chunk-block-stop ()
  "Test Anthropic streaming content_block_stop parsing."
  (let* ((backend (gpt-anthropic-create "test-key"))
         (chunk "{\"type\":\"content_block_stop\"}")
         (result (gpt-backend-parse-stream-chunk backend chunk 'thinking)))
    (should (null (plist-get result :state)))))

(ert-deftest gpt-test-anthropic-parse-stream-chunk-message-stop ()
  "Test Anthropic streaming message_stop parsing."
  (let* ((backend (gpt-anthropic-create "test-key"))
         (chunk "{\"type\":\"message_stop\"}")
         (result (gpt-backend-parse-stream-chunk backend chunk nil)))
    (should (plist-get result :done))))

;;; ============================================================
;;; Google backend tests
;;; ============================================================

(ert-deftest gpt-test-google-create ()
  "Test creating Google backend."
  (let ((backend (gpt-google-create "test-key")))
    (should (equal (oref backend name) "Google"))
    (should (equal (oref backend key) "test-key"))
    (should (member "gemini-3-pro-preview" (oref backend models)))))

(ert-deftest gpt-test-google-headers ()
  "Test Google header generation with API key."
  (let* ((backend (gpt-google-create "test-key"))
         (headers (gpt-backend-headers backend)))
    (should (assoc "x-goog-api-key" headers))
    (should (equal (cdr (assoc "x-goog-api-key" headers)) "test-key"))))

(ert-deftest gpt-test-google-build-url ()
  "Test Google URL building."
  (let* ((backend (gpt-google-create "test-key")))
    (let ((url (gpt-google--build-url backend "gemini-2.0-flash" nil)))
      (should (string-match-p "gemini-2.0-flash" url))
      (should (string-match-p "generateContent" url))
      ;; API key should NOT be in URL (it's in headers now)
      (should-not (string-match-p "key=" url)))
    (let ((stream-url (gpt-google--build-url backend "gemini-2.0-flash" t)))
      (should (string-match-p "streamGenerateContent" stream-url))
      (should (string-match-p "alt=sse" stream-url))
      (should-not (string-match-p "key=" stream-url)))))

(ert-deftest gpt-test-google-request-data-basic ()
  "Test Google basic request data building."
  (let* ((backend (gpt-google-create "test-key"))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "gemini-3-pro-preview" :max-tokens 100))
         (data (gpt-backend-request-data backend messages options)))
    (should (vectorp (plist-get data :contents)))
    (let ((gen-config (plist-get data :generationConfig)))
      (should (equal (plist-get gen-config :maxOutputTokens) 100)))))

(ert-deftest gpt-test-google-request-data-temperature ()
  "Test Google request data with temperature."
  (let* ((backend (gpt-google-create "test-key"))
         (messages '((:role "user" :content "Hello")))
         (options '(:model "gemini-2.0-flash" :max-tokens 100 :temperature 0.7))
         (data (gpt-backend-request-data backend messages options)))
    (let ((gen-config (plist-get data :generationConfig)))
      (should (equal (plist-get gen-config :temperature) 0.7)))))

(ert-deftest gpt-test-google-request-data-system-message ()
  "Test Google request data with system instruction."
  (let* ((backend (gpt-google-create "test-key"))
         (messages '((:role "system" :content "You are helpful")
                     (:role "user" :content "Hello")))
         (options '(:model "gemini-2.0-flash" :max-tokens 100))
         (data (gpt-backend-request-data backend messages options)))
    (should (plist-get data :systemInstruction))
    (let* ((sys-instr (plist-get data :systemInstruction))
           (parts (plist-get sys-instr :parts))
           (text (plist-get (aref parts 0) :text)))
      (should (equal text "You are helpful")))
    ;; Contents should not include system message
    (let ((contents (plist-get data :contents)))
      (should (= (length contents) 1)))))

(ert-deftest gpt-test-google-request-data-multi-turn ()
  "Test Google request data with multi-turn conversation."
  (let* ((backend (gpt-google-create "test-key"))
         (messages '((:role "user" :content "Hello")
                     (:role "assistant" :content "Hi!")
                     (:role "user" :content "How are you?")))
         (options '(:model "gemini-2.0-flash" :max-tokens 100))
         (data (gpt-backend-request-data backend messages options)))
    (let ((contents (plist-get data :contents)))
      (should (= (length contents) 3))
      (should (equal (plist-get (aref contents 0) :role) "user"))
      (should (equal (plist-get (aref contents 1) :role) "model"))
      (should (equal (plist-get (aref contents 2) :role) "user")))))

(ert-deftest gpt-test-google-parse-response ()
  "Test Google response parsing."
  (let* ((backend (gpt-google-create "test-key"))
         (response '(:candidates [(:content (:parts [(:text "Hello world")]))]))
         (content (gpt-backend-parse-response backend response)))
    (should (equal content "Hello world"))))

(ert-deftest gpt-test-google-parse-response-multi-part ()
  "Test Google response parsing with multiple parts."
  (let* ((backend (gpt-google-create "test-key"))
         (response '(:candidates [(:content (:parts [(:text "First ")
                                                     (:text "Second")]))]))
         (content (gpt-backend-parse-response backend response)))
    (should (equal content "First Second"))))

(ert-deftest gpt-test-google-parse-stream-chunk ()
  "Test Google streaming chunk parsing."
  (let* ((backend (gpt-google-create "test-key"))
         (chunk "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Hello\"}]}}]}")
         (result (gpt-backend-parse-stream-chunk backend chunk nil)))
    (should (equal (plist-get result :content) "Hello"))))

(ert-deftest gpt-test-google-parse-stream-chunk-done ()
  "Test Google streaming chunk parsing with finish reason."
  (let* ((backend (gpt-google-create "test-key"))
         (chunk "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Done\"}]},\"finishReason\":\"STOP\"}]}")
         (result (gpt-backend-parse-stream-chunk backend chunk nil)))
    (should (plist-get result :done))))

;;; ============================================================
;;; HTTP layer tests
;;; ============================================================

(ert-deftest gpt-test-http-curl-args ()
  "Test curl argument building."
  (let ((args (gpt-http--curl-args
               "https://api.example.com"
               '(("Authorization" . "Bearer test")
                 ("X-Custom" . "value")))))
    (should (member "--silent" args))
    (should (member "--no-buffer" args))
    (should (member "-X" args))
    (should (member "POST" args))
    (should (member "-H" args))
    (should (member "Authorization: Bearer test" args))
    (should (member "X-Custom: value" args))
    (should (member "https://api.example.com" args))))

(ert-deftest gpt-test-http-curl-available ()
  "Test curl availability check."
  ;; This test just verifies the function runs without error
  (should (or (gpt-http--curl-available-p)
              (not (gpt-http--curl-available-p)))))

;;; ============================================================
;;; Core module tests
;;; ============================================================

(ert-deftest gpt-test-model-api-lookup ()
  "Test looking up API type from model ID."
  (should (eq (gpt--get-model-api "gpt-5.2") 'openai))
  (should (eq (gpt--get-model-api "gpt-5.1") 'openai))
  (should (eq (gpt--get-model-api "gpt-5-mini") 'openai))
  (should (eq (gpt--get-model-api "claude-opus-4-6") 'anthropic))
  (should (eq (gpt--get-model-api "claude-sonnet-4-5") 'anthropic))
  (should (eq (gpt--get-model-api "gemini-3-pro-preview") 'google)))

(ert-deftest gpt-test-model-api-lookup-unknown ()
  "Test looking up API type for unknown model."
  (should (null (gpt--get-model-api "unknown-model"))))

(ert-deftest gpt-test-model-max-tokens ()
  "Test looking up max tokens from model ID."
  (should (equal (gpt--model-max-tokens "gpt-5.2") 400000))
  (should (equal (gpt--model-max-tokens "gpt-5-mini") 200000))
  (should (equal (gpt--model-max-tokens "claude-opus-4-6") 32000))
  (should (equal (gpt--model-max-tokens "claude-sonnet-4-5") 64000))
  (should (equal (gpt--model-max-tokens "gemini-3-pro-preview") 60000)))

(ert-deftest gpt-test-model-max-tokens-unknown ()
  "Test looking up max tokens for unknown model."
  (should (null (gpt--model-max-tokens "unknown-model"))))

(ert-deftest gpt-test-get-backend ()
  "Test backend creation and caching."
  ;; Set a test key
  (let ((gpt-openai-key "test-key")
        (gpt-backends nil))
    (let ((backend1 (gpt-get-backend 'openai))
          (backend2 (gpt-get-backend 'openai)))
      ;; Should return same cached instance
      (should (eq backend1 backend2))
      (should (equal (oref backend1 key) "test-key")))))

(ert-deftest gpt-test-backend-valid-p-openai ()
  "Test backend validation for OpenAI."
  (let ((backend (gpt-openai-create "test-key")))
    (should (gpt--backend-valid-p backend 'openai))
    (should-not (gpt--backend-valid-p backend 'anthropic))
    (should-not (gpt--backend-valid-p backend 'google))))

(ert-deftest gpt-test-backend-valid-p-anthropic ()
  "Test backend validation for Anthropic."
  (let ((backend (gpt-anthropic-create "test-key" :thinking-enabled nil)))
    (should (gpt--backend-valid-p backend 'anthropic))
    (should-not (gpt--backend-valid-p backend 'openai))
    (should-not (gpt--backend-valid-p backend 'google))))

(ert-deftest gpt-test-backend-valid-p-google ()
  "Test backend validation for Google."
  (let ((backend (gpt-google-create "test-key")))
    (should (gpt--backend-valid-p backend 'google))
    (should-not (gpt--backend-valid-p backend 'openai))
    (should-not (gpt--backend-valid-p backend 'anthropic))))

(ert-deftest gpt-test-backend-valid-p-nil ()
  "Test backend validation with nil."
  (should-not (gpt--backend-valid-p nil 'openai))
  (should-not (gpt--backend-valid-p nil 'anthropic))
  (should-not (gpt--backend-valid-p nil 'google)))

(ert-deftest gpt-test-backend-valid-p-garbage ()
  "Test backend validation with garbage values (symbols, strings, etc)."
  ;; Symbols should not be valid
  (should-not (gpt--backend-valid-p 'gpt-backend 'anthropic))
  (should-not (gpt--backend-valid-p 'some-symbol 'openai))
  ;; Strings should not be valid
  (should-not (gpt--backend-valid-p "string" 'google))
  ;; Numbers should not be valid
  (should-not (gpt--backend-valid-p 42 'anthropic))
  ;; Lists should not be valid
  (should-not (gpt--backend-valid-p '(a b c) 'openai)))

(ert-deftest gpt-test-get-backend-replaces-invalid ()
  "Test that gpt-get-backend replaces invalid cached backends."
  (let ((gpt-openai-key "test-key")
        (gpt-backends nil))
    ;; Manually cache an Anthropic backend under 'openai key (wrong type)
    (let ((wrong-backend (gpt-anthropic-create "wrong-key" :thinking-enabled nil)))
      (setf (alist-get 'openai gpt-backends) wrong-backend))
    ;; gpt-get-backend should detect the wrong type and create correct one
    (let ((backend (gpt-get-backend 'openai)))
      (should (cl-typep backend 'gpt-openai-backend))
      (should (equal (oref backend key) "test-key")))))

(ert-deftest gpt-test-get-backend-all-providers ()
  "Test backend creation for all providers."
  (let ((gpt-openai-key "openai-key")
        (gpt-anthropic-key "anthropic-key")
        (gpt-google-key "google-key")
        (gpt-thinking-enabled nil)
        (gpt-thinking-budget 10000)
        (gpt-interleaved-thinking nil)
        (gpt-web-search nil)
        (gpt-backends nil))
    ;; Test each provider
    (let ((openai (gpt-get-backend 'openai)))
      (should (cl-typep openai 'gpt-openai-backend)))
    (let ((anthropic (gpt-get-backend 'anthropic)))
      (should (cl-typep anthropic 'gpt-anthropic-backend)))
    (let ((google (gpt-get-backend 'google)))
      (should (cl-typep google 'gpt-google-backend)))))

(ert-deftest gpt-test-update-backend ()
  "Test that gpt-update-backend clears cache and recreates."
  (let ((gpt-openai-key "initial-key")
        (gpt-model "gpt-5.2")  ; OpenAI model
        (gpt-backends nil)
        (gpt-current-backend nil))
    ;; Create initial backend
    (setq gpt-current-backend (gpt-get-backend 'openai))
    (should (equal (oref gpt-current-backend key) "initial-key"))
    ;; Change key and update
    (setq gpt-openai-key "new-key")
    (gpt-update-backend)
    ;; Should have new key
    (should (equal (oref gpt-current-backend key) "new-key"))))

(ert-deftest gpt-test-validate-api-key-missing ()
  "Test API key validation with missing key."
  (let ((gpt-model "gpt-5.2")  ; OpenAI model
        (gpt-openai-key nil))
    (should-error (gpt-validate-api-key) :type 'user-error)))

(ert-deftest gpt-test-validate-api-key-empty ()
  "Test API key validation with empty key."
  (let ((gpt-model "claude-opus-4-5")  ; Anthropic model
        (gpt-anthropic-key ""))
    (should-error (gpt-validate-api-key) :type 'user-error)))

(ert-deftest gpt-test-validate-api-key-set ()
  "Test API key validation with valid key."
  (let ((gpt-model "gemini-3-pro-preview")  ; Google model
        (gpt-google-key "valid-key"))
    ;; Should not error
    (gpt-validate-api-key)))

;;; ============================================================
;;; HTTP error handling tests
;;; ============================================================

(ert-deftest gpt-test-http-retryable-status ()
  "Test identification of retryable HTTP status codes."
  (should (gpt-http--retryable-status-p 429))
  (should (gpt-http--retryable-status-p 500))
  (should (gpt-http--retryable-status-p 502))
  (should (gpt-http--retryable-status-p 503))
  (should (gpt-http--retryable-status-p 504))
  (should-not (gpt-http--retryable-status-p 200))
  (should-not (gpt-http--retryable-status-p 400))
  (should-not (gpt-http--retryable-status-p 401))
  (should-not (gpt-http--retryable-status-p 404)))

(ert-deftest gpt-test-http-parse-api-error-rate-limit ()
  "Test parsing rate limit errors."
  (let ((response '(:error (:type "rate_limit_error"
                            :message "Too many requests"))))
    (should (string-match-p "Rate limit"
                            (gpt-http--parse-api-error response 429)))))

(ert-deftest gpt-test-http-parse-api-error-auth ()
  "Test parsing authentication errors."
  (let ((response '(:error (:type "authentication_error"
                            :message "Invalid API key"))))
    (should (string-match-p "Authentication failed"
                            (gpt-http--parse-api-error response 401)))))

(ert-deftest gpt-test-http-parse-api-error-invalid-request ()
  "Test parsing invalid request errors."
  (let ((response '(:error (:type "invalid_request_error"
                            :message "Missing required parameter"))))
    (should (string-match-p "Invalid request"
                            (gpt-http--parse-api-error response 400)))))

(ert-deftest gpt-test-http-parse-api-error-model-not-found ()
  "Test parsing model not found errors."
  (let ((response '(:error (:message "Model not found: invalid-model"))))
    (should (string-match-p "Model not found"
                            (gpt-http--parse-api-error response 404)))))

(ert-deftest gpt-test-http-parse-api-error-server ()
  "Test parsing server errors."
  (let ((response '(:error (:message "Internal server error"))))
    (should (string-match-p "Server error"
                            (gpt-http--parse-api-error response 500)))))

(ert-deftest gpt-test-http-parse-api-error-overloaded ()
  "Test parsing overloaded errors."
  (let ((response '(:error (:type "overloaded_error"
                            :message "API is overloaded"))))
    (should (string-match-p "overloaded"
                            (gpt-http--parse-api-error response 529)))))

(ert-deftest gpt-test-http-parse-api-error-with-code ()
  "Test parsing errors with error code."
  (let ((response '(:error (:message "Something went wrong"
                            :code "RESOURCE_EXHAUSTED"))))
    (should (string-match-p "RESOURCE_EXHAUSTED"
                            (gpt-http--parse-api-error response 403)))))

;;; ============================================================
;;; Search/Replace editing tests
;;; ============================================================

(require 'gpt-ui)

(ert-deftest gpt-test-edit-parse-search-replace-single ()
  "Test parsing a single search/replace block."
  (let* ((text "Some text before\n<<<<<<< SEARCH\nold code\n=======\nnew code\n>>>>>>> REPLACE\nSome text after")
         (blocks (gpt--edit-parse-search-replace-blocks text)))
    (should (= (length blocks) 1))
    (should (equal (plist-get (car blocks) :search) "old code"))
    (should (equal (plist-get (car blocks) :replace) "new code"))))

(ert-deftest gpt-test-edit-parse-search-replace-multiple ()
  "Test parsing multiple search/replace blocks."
  (let* ((text "<<<<<<< SEARCH\nfirst old\n=======\nfirst new\n>>>>>>> REPLACE\n\n<<<<<<< SEARCH\nsecond old\n=======\nsecond new\n>>>>>>> REPLACE")
         (blocks (gpt--edit-parse-search-replace-blocks text)))
    (should (= (length blocks) 2))
    (should (equal (plist-get (car blocks) :search) "first old"))
    (should (equal (plist-get (car blocks) :replace) "first new"))
    (should (equal (plist-get (cadr blocks) :search) "second old"))
    (should (equal (plist-get (cadr blocks) :replace) "second new"))))

(ert-deftest gpt-test-edit-parse-search-replace-multiline ()
  "Test parsing search/replace with multiline content."
  (let* ((text "<<<<<<< SEARCH\nline 1\nline 2\nline 3\n=======\nnew line 1\nnew line 2\n>>>>>>> REPLACE")
         (blocks (gpt--edit-parse-search-replace-blocks text)))
    (should (= (length blocks) 1))
    (should (equal (plist-get (car blocks) :search) "line 1\nline 2\nline 3"))
    (should (equal (plist-get (car blocks) :replace) "new line 1\nnew line 2"))))

(ert-deftest gpt-test-edit-parse-search-replace-empty-replace ()
  "Test parsing search/replace with empty replacement (deletion)."
  (let* ((text "<<<<<<< SEARCH\ndelete me\n=======\n>>>>>>> REPLACE")
         (blocks (gpt--edit-parse-search-replace-blocks text)))
    (should (= (length blocks) 1))
    (should (equal (plist-get (car blocks) :search) "delete me"))
    (should (equal (plist-get (car blocks) :replace) ""))))

(ert-deftest gpt-test-edit-has-search-replace-blocks ()
  "Test detection of search/replace blocks."
  (should (gpt--edit-has-search-replace-blocks "<<<<<<< SEARCH\nfoo\n=======\nbar\n>>>>>>> REPLACE"))
  (should (gpt--edit-has-search-replace-blocks "text before <<<<<<< SEARCH"))
  (should-not (gpt--edit-has-search-replace-blocks "just regular text"))
  (should-not (gpt--edit-has-search-replace-blocks "```\ncode block\n```")))

(ert-deftest gpt-test-edit-find-match-exact ()
  "Test exact matching in content."
  (let ((content "line 1\nfind me\nline 3"))
    (let ((result (gpt--edit-find-match "find me" content)))
      (should (equal (plist-get result :match-type) 'exact))
      (should (= (plist-get result :start) 7))
      (should (= (plist-get result :end) 14)))))

(ert-deftest gpt-test-edit-find-match-not-found ()
  "Test when search text is not found."
  (let ((content "line 1\nline 2\nline 3"))
    (let ((result (gpt--edit-find-match "not here" content)))
      (should (equal (plist-get result :error) 'not-found)))))

(ert-deftest gpt-test-edit-find-match-multiple ()
  "Test when search text appears multiple times."
  (let ((content "foo\nbar\nfoo\nbaz"))
    (let ((result (gpt--edit-find-match "foo" content)))
      (should (equal (plist-get result :error) 'multiple-matches))
      (should (= (plist-get result :count) 2)))))

(ert-deftest gpt-test-edit-find-match-fuzzy ()
  "Test fuzzy matching with whitespace differences."
  ;; Fuzzy matching normalizes whitespace per-line and matches
  ;; Content has extra trailing whitespace that exact match won't find
  (let ((content "line 1\ndef foo():  \n  return 1  \nline 4"))
    ;; Search for the same code without trailing spaces - won't exact match
    ;; because "def foo():  " != "def foo():" but fuzzy normalizes both
    (let ((result (gpt--edit-find-match "def foo():\n  return 1" content)))
      (should result)
      (should (not (plist-get result :error)))
      ;; Note: may be exact or fuzzy depending on substring matching
      ;; The key is that it finds it
      (should (plist-get result :start)))))

(ert-deftest gpt-test-edit-apply-search-replace-simple ()
  "Test applying a single search/replace."
  (let* ((content "hello world")
         (blocks (list (list :search "world" :replace "everyone")))
         (result (gpt--edit-apply-search-replace content blocks)))
    (should (equal (plist-get result :content) "hello everyone"))
    (should (= (plist-get result :applied) 1))
    (should (null (plist-get result :errors)))))

(ert-deftest gpt-test-edit-apply-search-replace-multiple ()
  "Test applying multiple search/replace blocks."
  (let* ((content "foo bar baz")
         (blocks (list (list :search "foo" :replace "FOO")
                       (list :search "baz" :replace "BAZ")))
         (result (gpt--edit-apply-search-replace content blocks)))
    (should (equal (plist-get result :content) "FOO bar BAZ"))
    (should (= (plist-get result :applied) 2))))

(ert-deftest gpt-test-edit-apply-search-replace-deletion ()
  "Test applying a deletion (empty replacement)."
  (let* ((content "keep delete keep")
         (blocks (list (list :search " delete" :replace "")))
         (result (gpt--edit-apply-search-replace content blocks)))
    (should (equal (plist-get result :content) "keep keep"))
    (should (= (plist-get result :applied) 1))))

(ert-deftest gpt-test-edit-apply-search-replace-with-errors ()
  "Test applying blocks where some fail."
  (let* ((content "hello world")
         (blocks (list (list :search "world" :replace "everyone")
                       (list :search "notfound" :replace "x")))
         (result (gpt--edit-apply-search-replace content blocks)))
    (should (equal (plist-get result :content) "hello everyone"))
    (should (= (plist-get result :applied) 1))
    (should (= (length (plist-get result :errors)) 1))))

(ert-deftest gpt-test-edit-normalize-whitespace ()
  "Test whitespace normalization for fuzzy matching."
  (should (equal (gpt--edit-normalize-whitespace "  foo  ") "foo"))
  (should (equal (gpt--edit-normalize-whitespace "  line 1  \n  line 2  ") "line 1\nline 2")))

;;; ============================================================
;;; Method dispatch tests (ensures all backend methods are registered)
;;; ============================================================

(require 'gpt)

(ert-deftest gpt-test-all-backend-methods-registered ()
  "Verify that loading gpt.el registers all backend method specializations.
This catches the class of bug where backend modules are not eagerly loaded,
causing cl-no-applicable-method errors at runtime."
  (let ((gpt-openai-key "test")
        (gpt-anthropic-key "test")
        (gpt-google-key "test")
        (gpt-thinking-enabled nil)
        (gpt-thinking-budget 10000)
        (gpt-interleaved-thinking nil)
        (gpt-web-search nil)
        (gpt-backends nil))
    ;; Test each provider's full method chain
    (dolist (provider '(openai anthropic google))
      (let* ((backend (gpt-get-backend provider))
             (messages '((:role "user" :content "test")))
             (options '(:model "test" :max-tokens 100 :thinking-enabled nil)))
        ;; All four generic methods must dispatch without error
        (should (gpt-backend-headers backend))
        (should (gpt-backend-request-data backend messages options))
        (should (gpt-backend-stream-request-data backend messages options))))))

(ert-deftest gpt-test-anthropic-stream-method-dispatch ()
  "Verify gpt-backend-stream-request-data dispatches for Anthropic backends.
Regression test for cl-no-applicable-method bug caused by lazy loading."
  (let* ((backend (gpt-anthropic-create "test-key" :thinking-enabled nil))
         (messages '((:role "user" :content "hello")))
         (options '(:model "claude-opus-4-6" :max-tokens 32000 :thinking-enabled nil))
         (data (gpt-backend-stream-request-data backend messages options)))
    (should (eq (plist-get data :stream) t))
    (should (equal (plist-get data :model) "claude-opus-4-6"))))

(provide 'gpt-test)
;;; gpt-test.el ends here
