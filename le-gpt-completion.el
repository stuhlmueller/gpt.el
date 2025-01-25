;;; le-gpt-completion.el --- Completion functionality for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;

;;; Code:

(require 'le-gpt-core)
(require 'le-gpt-project)

(defcustom le-gpt-complete-at-point-instructions "Provide a short completion to be inserted at <cursor>. Only provide the completion, no commentary, no quotes, no code blocks. Your response will directly be inserted."
  "The instructions for gpt to perform completion at point without any noise."
  :type 'string
  :group 'le-gpt)

(defun le-gpt-completion-at-point (temp-context-files)
  "Get completion from GPT based on buffer content up to point.
If TEMP-CONTEXT-FILES is non-nil, prompt for context files.
The generated completion is displayed directly in buffer."
  (let* ((start-point (point))
         (buffer-content (buffer-substring-no-properties (point-min) start-point))
         (buffer-rest (buffer-substring-no-properties start-point (point-max)))
         (project-context (le-gpt--get-project-context temp-context-files))
         (prompt (concat (when project-context (concat "User:\n\n" project-context))
                         "User: " buffer-content "<cursor>" buffer-rest "\n\nGPTInstructions: " le-gpt-complete-at-point-instructions))
         (prompt-file (le-gpt--create-prompt-file prompt))
         (insertion-marker (make-marker))
         (process (le-gpt--make-process prompt-file nil)))
    (set-marker insertion-marker (point))
    (set-process-filter process (lambda (proc string)
                                  (ignore proc)
                                  (save-excursion
                                    (goto-char insertion-marker)
                                    (insert string)
                                    (set-marker insertion-marker (point)))))))

(provide 'le-gpt-completion)
;;; le-gpt-completion.el ends here
