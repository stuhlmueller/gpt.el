;;; gpt-pilot-transform.el --- Transform region functionality for gpt-pilot.el -*- lexical-binding: t; -*-
(require 'gpt-pilot-core)
(require 'gpt-pilot-project)

(defvar gpt-pilot-transform-region-instructions
  "You should transform what is inside <region>. Only change what was requested without anything else, e.g., no explanatory comments, no triple backticks. Your response will replace what is inside region as is."
  "The instruction to give gpt so that it performs the transformation as intended.")

(defun gpt-pilot-transform-region-with-prompt ()
  "Transform the selected region.
Ask the user for the transformation and then replace the selected region by the response."
  (let* ((start (region-beginning))
         (end (region-end))
         (region-content (buffer-substring-no-properties start end))
         (buffer-before (buffer-substring-no-properties (point-min) start))
         (buffer-after (buffer-substring-no-properties end (point-max)))
         (command (gpt-pilot-read-command))
         (project-context (when gpt-pilot-project-file-context
                            (format gpt-pilot-project-context-format
                                    (mapconcat #'identity gpt-pilot-project-file-context "\n")
                                    (gpt-pilot-get-file-contents gpt-pilot-project-file-context))))
         (prompt (concat (when project-context (concat "User:\n\n" project-context))
                         "User: " command "\n"
                         "<region>" region-content "<region>" "\n"
                         gpt-pilot-transform-region-instructions "\n"
                         "GPTContext: " buffer-before "\n" buffer-after))
         (prompt-file (gpt-pilot-create-prompt-file prompt))
         (insertion-marker (make-marker))
         (process (gpt-pilot-make-process prompt-file nil)))
    (delete-region start end)
    (set-marker insertion-marker (point))
    (set-process-filter process (lambda (proc string)
                                  (save-excursion
                                    (goto-char insertion-marker)
                                    (insert string)
                                    (set-marker insertion-marker (point)))))))


(provide 'gpt-pilot-transform)
;;; gpt-pilot-transform.el ends here
