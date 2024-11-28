;;; gpt-pilot-completion.el --- Completion functionality for gpt-pilot.el -*- lexical-binding: t; -*-
(require 'gpt-pilot-core)
(require 'gpt-pilot-project)

(defface gpt-pilot-completion-preview-face
  '((t :inherit current :underline t :weight bold))
  "Face for previewing code completions.")

(defvar gpt-pilot-complete-at-point-instructions "Provide a short completion to be inserted at <cursor>. Only provide the completion, no commentary, no quotes. Your response will directly be inserted."
  "The instructions to give gpt so that it performs completion at point without any noise.")

(defun gpt-pilot-completion-at-point ()
  "Get completion from gpt based on buffer content up to point.
The generated completion is displayed directly in buffer and can be accepted with RET."
  (let* ((start-point (point))
         (overlay (make-overlay start-point start-point))
         (buffer-content (buffer-substring-no-properties (point-min) start-point))
         (buffer-rest (buffer-substring-no-properties start-point (point-max)))
         (project-context (when gpt-pilot-project-file-context
                            (format gpt-pilot-project-context-format
                                    (mapconcat #'identity gpt-pilot-project-file-context "\n")
                                    (gpt-pilot-get-file-contents gpt-pilot-project-file-context))))
         (prompt (concat (when project-context (concat "User:\n\n" project-context))
                         "User: " buffer-content "<cursor>" buffer-rest "\n\nUser: " gpt-pilot-complete-at-point-instructions))
         (prompt-file (gpt-pilot-create-prompt-file prompt))
         (insertion-marker (make-marker))
         (process (gpt-pilot-make-process prompt-file nil)))
    (overlay-put overlay 'face 'gpt-pilot-completion-preview-face)
    (set-marker insertion-marker (point))
    (set-process-filter process (lambda (proc string)
                                  (save-excursion
                                    (goto-char insertion-marker)
                                    (insert string)
                                    ;; Update the overlay to cover the new text
                                    (move-overlay overlay start-point (point))
                                    (set-marker insertion-marker (point)))))
    ;; Wait for user confirmation
    (let ((response (read-key (format "Press RET to accept completion, any other key to cancel"))))
      (if (eq response ?\r)
          (delete-overlay overlay)  ; Remove overlay if accepted
        (delete-region start-point (point))  ; Remove text if canceled
        (delete-overlay overlay)
        (message "Completion canceled")))))

(provide 'gpt-pilot-completion)
;;; gpt-pilot-completion.el ends here
