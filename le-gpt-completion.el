;;; le-gpt-completion.el --- Completion functionality for le-gpt.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; 

;;; Code:

(require 'le-gpt-core)
(require 'le-gpt-project)

(defface le-gpt-completion-preview-face
  '((t :inherit current :underline t :weight bold))
  "Face for previewing code completions.")

(defcustom le-gpt-complete-at-point-instructions "Provide a short completion to be inserted at <cursor>. Only provide the completion, no commentary, no quotes, no code blocks. Your response will directly be inserted."
  "The instructions for gpt to perform completion at point without any noise."
  :type 'string
  :group 'le-gpt)

(defun le-gpt-completion-at-point ()
  "Get completion from gpt based on buffer content up to point.
The generated completion is displayed directly in buffer.  Accept with RET."
  (let* ((start-point (point))
         (overlay (make-overlay start-point start-point))
         (buffer-content (buffer-substring-no-properties (point-min) start-point))
         (buffer-rest (buffer-substring-no-properties start-point (point-max)))
         (project-context (le-gpt-get-project-context))
         (prompt (concat (when project-context (concat "User:\n\n" project-context))
                         "User: " buffer-content "<cursor>" buffer-rest "\n\nUser: " le-gpt-complete-at-point-instructions))
         (prompt-file (le-gpt--create-prompt-file prompt))
         (insertion-marker (make-marker))
         (process (le-gpt--make-process prompt-file nil)))
    (overlay-put overlay 'face 'le-gpt-completion-preview-face)
    (set-marker insertion-marker (point))
    (set-process-filter process (lambda (proc string)
                                  (ignore proc)
                                  (save-excursion
                                    (goto-char insertion-marker)
                                    (insert string)
                                    ;; Update the overlay to cover the new text
                                    (move-overlay overlay start-point (point))
                                    (set-marker insertion-marker (point)))))
    ;; Wait for user confirmation
    (let ((response (read-key (format "Press RET to accept completion, any other key to cancel"))))
      (delete-overlay overlay)
      (if (eq response ?\r)
          (insert "\n") ;; add a newline for good measure
        (delete-region start-point (point))))))  ; Remove text if canceled


(provide 'le-gpt-completion)
;;; le-gpt-completion.el ends here
