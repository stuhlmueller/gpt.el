;;; le-gpt-consult.el --- Consult integration for le-gpt-chat -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides consult integration for searching through GPT chat buffers.

;;; Code:

(require 'le-gpt-chat)

;; Declare consult functions to avoid warnings
(declare-function consult--read "consult")
(declare-function consult--dynamic-collection "consult")
(declare-function consult--default-regexp-compiler "consult")
(declare-function consult--tofu-encode "consult")
(declare-function consult--lookup-member "consult")

(defcustom le-gpt-consult-search-min-chars 3
  "Minimum number of characters before starting search in consult."
  :type 'integer
  :group 'le-gpt)

(defvar le-gpt-consult--history nil
  "History for GPT buffer search.")

;; Private functions
(defun le-gpt-consult--group (cand transform)
  "Return buffer name for CAND or TRANSFORM the candidate."
  (if transform
      cand
    (when-let ((info (get-text-property 0 'le-gpt--info cand)))
      (car info)))) ; Return buffer name for grouping

(defun le-gpt-consult--candidates (buffers input)
  "Collect matching candidates from GPT buffers.
INPUT is the user input which should be matched.
BUFFERS is the list of GPT buffers.
Returns list of candidates."
  (if (< (length (string-trim input)) le-gpt-consult-search-min-chars)
      nil
    (pcase-let* ((`(,regexps . ,hl) (consult--default-regexp-compiler input 'emacs t))
                 (candidates nil)
                 (cand-idx 0))
      (when regexps
        (dolist (buf buffers)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (save-excursion
                (goto-char (point-min))
                (let ((line-num 1))
                  (while (not (eobp))
                    (let* ((bol (line-beginning-position))
                           (eol (line-end-position))
                           (line-content (buffer-substring-no-properties bol eol))
                           (trimmed-content (string-trim line-content)))
                      ;; Skip empty lines and check if all regexps match
                      (when (and (not (string-empty-p trimmed-content))
                                 (cl-loop for r in regexps always
                                          (string-match-p r line-content)))
                        (let ((cand (concat
                                     (funcall hl trimmed-content)
                                     (consult--tofu-encode cand-idx))))
                          (put-text-property 0 1 'le-gpt--info
                                             (list (buffer-name buf) line-num bol buf) cand)
                          (cl-incf cand-idx)
                          (push cand candidates))))
                    (forward-line 1)
                    (setq line-num (1+ line-num))))))))
        (nreverse candidates)))))

(defun le-gpt-consult--action (cand)
  "Jump to GPT buffer location for CAND."
  (when-let* ((info (get-text-property 0 'le-gpt--info cand))
              (buffer-name (nth 0 info))
              (line-num (nth 1 info))
              (bol (nth 2 info))
              (buf (nth 3 info)))
    (when (buffer-live-p buf)
      (switch-to-buffer buf)
      (goto-char bol)
      (recenter)
      ;; Briefly highlight the line
      (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
        (overlay-put ov 'face 'highlight)
        (run-with-timer 1.0 nil
                        (lambda (overlay)
                          (when (overlayp overlay)
                            (delete-overlay overlay)))
                        ov))
      (run-hooks 'consult-after-jump-hook))))

(defun le-gpt-consult--state ()
  "GPT buffer preview state."
  (lambda (action cand)
    (pcase action
      ('preview
       (when-let* ((info (get-text-property 0 'le-gpt--info cand))
                   (buffer-name (nth 0 info))
                   (line-num (nth 1 info))
                   (bol (nth 2 info))
                   (buf (nth 3 info)))
         (when (buffer-live-p buf)
           (with-selected-window
               (or (get-buffer-window buf)
                   (display-buffer buf '(display-buffer-pop-up-window)))
             (goto-char bol)
             (recenter)
             ;; Highlight the preview line briefly
             (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
               (overlay-put ov 'face 'consult-preview-line)
               (run-with-timer 0.3 nil
                               (lambda (overlay)
                                 (when (overlayp overlay)
                                   (delete-overlay overlay)))
                               ov))))))
      ('return
       (le-gpt-consult--action cand)))))

;; Public functions
(defun le-gpt-consult-buffers ()
  "Search through GPT buffer contents using consult."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "Consult package not available"))

  (let ((gpt-buffers (le-gpt--get-gpt-buffers)))
    (if (null gpt-buffers)
        (message "No GPT buffers found")
      (consult--read
       (consult--dynamic-collection
        (lambda (input)
          (le-gpt-consult--candidates gpt-buffers input)))
       :state (le-gpt-consult--state)
       :prompt "Search GPT buffers: "
       :require-match t
       :sort nil
       :category 'le-gpt-search
       :history '(:input le-gpt-consult--history)
       :group #'le-gpt-consult--group
       :add-history (thing-at-point 'symbol)
       :lookup #'consult--lookup-member))))

(provide 'le-gpt-consult)
;;; le-gpt-consult.el ends here
