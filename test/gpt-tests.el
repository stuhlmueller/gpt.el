;;; gpt-tests.el --- ERT tests for gpt.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andreas Stuhlmueller

;; Author: Test Suite
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Basic regression tests for helper functions in gpt.el.

;;; Code:

(require 'ert)
(require 'gpt-ui)
(require 'gpt-core)

(ert-deftest gpt-clean-edit-output-strips-thinking-and-prefix ()
  "Ensure thinking markers and assistant prefixes are removed."
  (let ((input "[Thinking...]\nPlan\n[Thinking done.]\nAssistant: Final text"))
    (should (equal (gpt--clean-edit-output input) "Final text"))))

(ert-deftest gpt-get-context-current-buffer-with-region ()
  "Context should include cursor marker and selected region."
  (with-temp-buffer
    (insert "first line\nsecond line")
    (goto-char (point-min))
    (push-mark (point) t t)
    (forward-word 2)
    (let* ((transient-mark-mode t)
           (mark-active t)
           (context (gpt-get-context 'current-buffer)))
      (should (string-match "<cursor/>" context))
      (should (string-match "Selected region:" context))
      (should (string-match "first line" context)))))

(ert-deftest gpt-format-command-with-model-prefixes-command ()
  "Multi-model helper should prefix the command with the model id."
  (should (equal (gpt--format-command-with-model "Do things" "gpt-5")
                 "[gpt-5] Do things")))

;; --- Process management tests ---

(require 'gpt-api)
(require 'gpt-mode)

(ert-deftest gpt-create-prompt-file-creates-temp-file ()
  "Verify gpt-create-prompt-file creates a temp file with buffer contents."
  (with-temp-buffer
    (insert "Test prompt content")
    (let ((temp-file (gpt-create-prompt-file (current-buffer))))
      (unwind-protect
          (progn
            (should (file-exists-p temp-file))
            (should (string-prefix-p "gpt-prompt-" (file-name-nondirectory temp-file)))
            (should (equal "Test prompt content"
                           (with-temp-buffer
                             (insert-file-contents temp-file)
                             (buffer-string)))))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest gpt-message-formats-correctly ()
  "Verify gpt-message prepends GPT prefix."
  (should (equal "GPT: Hello world"
                 (let ((messages nil))
                   (cl-letf (((symbol-function 'message)
                              (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
                     (gpt-message "Hello %s" "world"))
                   (car messages)))))

(ert-deftest gpt-buffer-string-returns-content ()
  "Verify gpt-buffer-string returns buffer text."
  (with-temp-buffer
    (insert "Buffer content here")
    (should (equal "Buffer content here" (gpt-buffer-string (current-buffer))))))

(ert-deftest gpt-start-process-validates-api-key ()
  "Verify gpt-start-process errors when API key is nil."
  (let ((gpt-api-type 'openai)
        (gpt-openai-key nil)
        (gpt-script-path (expand-file-name "gpt.py" (file-name-directory (or load-file-name buffer-file-name default-directory)))))
    (with-temp-buffer
      (let ((prompt-file (make-temp-file "gpt-test-")))
        (unwind-protect
            (should-error (gpt-start-process prompt-file (current-buffer))
                          :type 'user-error)
          (when (file-exists-p prompt-file)
            (delete-file prompt-file)))))))

(ert-deftest gpt-start-process-validates-script-path ()
  "Verify gpt-start-process errors when script doesn't exist."
  (let ((gpt-api-type 'openai)
        (gpt-openai-key "test-key")
        (gpt-script-path "/nonexistent/path/gpt.py"))
    (with-temp-buffer
      (let ((prompt-file (make-temp-file "gpt-test-")))
        (unwind-protect
            (should-error (gpt-start-process prompt-file (current-buffer))
                          :type 'user-error)
          (when (file-exists-p prompt-file)
            (delete-file prompt-file)))))))

(ert-deftest gpt-set-process-sentinel-cleans-up-files ()
  "Verify process sentinel deletes prompt file on exit."
  (let ((prompt-file (make-temp-file "gpt-sentinel-test-"))
        (cleanup-called nil))
    ;; Create a mock process
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (mock-proc (start-process "test-proc" buf "true")))
        ;; Set up sentinel
        (gpt-set-process-sentinel mock-proc nil prompt-file)
        ;; Wait for process to finish
        (while (process-live-p mock-proc)
          (sit-for 0.01))
        ;; Give sentinel time to run
        (sit-for 0.1)
        ;; Verify file was deleted
        (should-not (file-exists-p prompt-file))))))

(ert-deftest gpt-kill-process-stops-running-process ()
  "Verify gpt-kill-process terminates the buffer's process."
  (with-temp-buffer
    (gpt-mode)
    ;; Start a long-running process
    (let ((proc (start-process "test-sleep" (current-buffer) "sleep" "10")))
      (should (process-live-p proc))
      ;; Kill it
      (gpt-kill-process)
      ;; Verify it's dead
      (should-not (process-live-p proc)))))

(provide 'gpt-tests)

;;; gpt-tests.el ends here
