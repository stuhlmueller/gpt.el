;;; le-gpt-transform.el --- Transform region functionality for le-gpt.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; 

;;; Code:

(require 'le-gpt-core)
(require 'le-gpt-project)

(defcustom le-gpt-transform-region-instructions
  "You should transform what is inside <region>. Only change what was requested without anything else, e.g., no explanatory comments, no triple backticks. Your response will replace what is inside region as is."
  "The instruction to give gpt so that it performs the transformation as intended."
  :type 'string
  :group 'le-gpt)

(defun le-gpt-transform-region-with-prompt ()
  "Transform the selected region.
Ask user for the transformation command and replace region with response."
  (let* ((start (region-beginning))
         (end (region-end))
         (region-content (buffer-substring-no-properties start end))
         (buffer-before (buffer-substring-no-properties (point-min) start))
         (buffer-after (buffer-substring-no-properties end (point-max)))
         (command (le-gpt--read-command))
         (project-context (le-gpt-get-project-context))
         (prompt (concat (when project-context (concat "User:\n\n" project-context))
                         "User: " command "\n"
                         "<region>" region-content "<region>" "\n"
                         le-gpt-transform-region-instructions "\n"
                         "GPTContext: " buffer-before "\n" buffer-after))
         (prompt-file (le-gpt--create-prompt-file prompt))
         (insertion-marker (make-marker))
         (process (le-gpt--make-process prompt-file nil)))
    (delete-region start end)
    (set-marker insertion-marker (point))
    (set-process-filter process (lambda (proc string)
                                  (ignore proc)
                                  (save-excursion
                                    (goto-char insertion-marker)
                                    (insert string)
                                    (set-marker insertion-marker (point)))))))

(provide 'le-gpt-transform)
;;; le-gpt-transform.el ends here
