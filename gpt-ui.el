;;; gpt-ui.el --- UI functionality for gpt.el -*- lexical-binding: t; package-lint-main-file: "gpt.el"; -*-

;; Copyright (C) 2022-2025 Andreas Stuhlmueller

;; Author: Andreas Stuhlmueller <emacs@stuhlmueller.org>
;; Version: 3.0
;; Keywords: openai, anthropic, claude, language, copilot, convenience, tools
;; URL: https://github.com/stuhlmueller/gpt.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains user interface functions and buffer management for gpt.el.
;; This is the pure Elisp version - no Python dependencies.

;;; Code:

(require 'gpt-core)
(require 'gpt-api)
(require 'cl-lib)
(require 'subr-x)
(require 'diff)

(declare-function gpt-mode "gpt-mode" nil)

(defvar gpt-buffer-counter 0
  "Counter to ensure unique buffer names for GPT output buffers.")

(defcustom gpt-buffer-name-length 60
  "Maximum character length of the GPT buffer name title.
Buffer names are truncated to this length with '...' appended if needed."
  :type 'integer
  :group 'gpt)

(defun gpt-read-command (context-mode use-selection)
  "Read a GPT command from the user with history and completion.
Shows CONTEXT-MODE and selection status in the prompt.
USE-SELECTION determines whether selection will be included."
  (let* ((has-region (and use-selection (use-region-p)))
         (context-desc
          (cond
           ((eq context-mode 'all-buffers)
            (if has-region "all buffers + selection" "all buffers"))
           ((eq context-mode 'current-buffer)
            (if has-region "buffer + selection" "buffer"))
           ((and (null context-mode) has-region) "selection")
           (t nil)))
         (prompt (if context-desc
                     (format "%s [%s]: " gpt-model context-desc)
                   (format "%s: " gpt-model)))
         (cmd (gpt-read-command-with-space prompt
                                         gpt-command-history nil nil nil
                                         'gpt-command-history)))
    (if (string-equal cmd "n/a")
        ""
      (string-trim cmd))))

(defun gpt-get-buffer-content (buffer &optional include-metadata)
  "Get content from BUFFER.
Optionally include metadata if INCLUDE-METADATA is non-nil."
  (with-current-buffer buffer
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (if include-metadata
          (format "# %s (File %s)\n\n%s"
                  (buffer-name)
                  (or (buffer-file-name) "N/A")
                  content)
        content))))

(defun gpt-get-buffer-content-with-cursor (buffer &optional include-metadata)
  "Get content from BUFFER with a <cursor/> inserted at point.
If INCLUDE-METADATA is non-nil, prepend a header with buffer name and file path."
  (with-current-buffer buffer
    (let* ((before (buffer-substring-no-properties (point-min) (point)))
           (after (buffer-substring-no-properties (point) (point-max))))
      (if include-metadata
          (format "# %s (File %s)\n\n%s<cursor/>%s"
                  (buffer-name)
                  (or (buffer-file-name) "N/A")
                  before
                  after)
        (concat before "<cursor/>" after)))))

(defun gpt--mode-language (mode)
  "Derive LANGUAGE string for MODE suitable for markdown fences."
  (let ((name (string-remove-suffix "-mode" (symbol-name mode))))
    (if (string-empty-p name) "text" name)))

(defun gpt--edit-extract-code-block (text)
  "Extract and return first fenced code block body from TEXT.
If no code fence is found, return TEXT trimmed."
  (if (string-match "```[^\n]*\n\\([\\s\\S]*?\\)```" text)
      (match-string 1 text)
    (string-trim text)))

(defun gpt--edit-remove-thinking-blocks (text)
  "Strip [Thinking...] sections and related markers from TEXT."
  (let ((case-fold-search nil)
        (pattern "\\[Thinking\\.\\.\\.\\]\\(?:.\\|\n\\)*?\\[Thinking done\\.\\]"))
    (while (string-match pattern text)
      (setq text (concat (substring text 0 (match-beginning 0))
                         (substring text (match-end 0)))))
    (setq text (replace-regexp-in-string "^\\[Thinking[^]]*\\]\\s-*" "" text))
    (setq text (replace-regexp-in-string "\\s-*\\[Thinking done\\.\\]\\s-*" "" text))
    text))

(defun gpt--edit-sanitize-utf8 (text)
  "Remove characters from TEXT that cannot be safely encoded as UTF-8."
  (apply #'string
         (cl-loop for ch across text
                  unless (or (< ch 0)
                             (>= ch #x110000)
                             (and (>= ch #xD800) (<= ch #xDFFF)))
                  collect ch)))

(defun gpt--clean-edit-output (text)
  "Normalize GPT edit output TEXT by removing meta markers."
  (let* ((without-thinking (gpt--edit-remove-thinking-blocks text))
         (stripped (string-trim without-thinking)))
    (setq stripped (replace-regexp-in-string "^Assistant:\\s-*" "" stripped))
    (gpt--edit-sanitize-utf8 stripped)))

(defun gpt--edit-strip-code-fences (text)
  "Remove leading and trailing Markdown code fences from TEXT."
  (let ((clean text))
    (when (string-match "\\`[ \t]*```[^\n]*\n" clean)
      (setq clean (substring clean (match-end 0))))
    (cond
     ((string-match "\n```[^\n]*[ \t]*\\'" clean)
      (setq clean (substring clean 0 (match-beginning 0))))
     ((string-match "```[^\n]*[ \t]*\\'" clean)
      (setq clean (substring clean 0 (match-beginning 0)))))
    clean))

;;; Search/Replace block editing

(defun gpt--edit-parse-search-replace-blocks (text)
  "Parse TEXT for SEARCH/REPLACE blocks.
Returns a list of plists with :search and :replace keys.
Format expected:
<<<<<<< SEARCH
old code
=======
new code
>>>>>>> REPLACE"
  (let ((blocks '())
        (pos 0)
        (pattern (concat "<<<<<<+[ \t]*SEARCH[ \t]*\n"
                         "\\(\\(?:.*\n\\)*?\\)"
                         "=======*[ \t]*\n"
                         "\\(\\(?:.*\n\\)*?\\)"
                         ">>>>>>>+[ \t]*REPLACE")))
    (while (string-match pattern text pos)
      (let ((search-text (match-string 1 text))
            (replace-text (match-string 2 text)))
        ;; Remove trailing newline from search (but keep internal structure)
        (when (string-suffix-p "\n" search-text)
          (setq search-text (substring search-text 0 -1)))
        ;; Remove trailing newline from replace
        (when (string-suffix-p "\n" replace-text)
          (setq replace-text (substring replace-text 0 -1)))
        (push (list :search search-text :replace replace-text) blocks))
      (setq pos (match-end 0)))
    (nreverse blocks)))

(defun gpt--edit-has-search-replace-blocks (text)
  "Return non-nil if TEXT contains SEARCH/REPLACE blocks."
  (string-match-p "<<<<<<+[ \t]*SEARCH" text))

(defun gpt--edit-normalize-whitespace (text)
  "Normalize TEXT for fuzzy matching by collapsing whitespace."
  (let ((normalized text))
    ;; Trim each line
    (setq normalized
          (mapconcat (lambda (line) (string-trim line))
                     (split-string normalized "\n")
                     "\n"))
    ;; Trim overall
    (string-trim normalized)))

(defun gpt--edit-find-match (search-text content)
  "Find SEARCH-TEXT in CONTENT.
Returns a plist with :start, :end, and :match-type keys.
:match-type is \\='exact or \\='fuzzy.
Returns nil if no unique match found."
  (let ((exact-matches '())
        (pos 0))
    ;; Try exact match first
    (while (setq pos (string-match (regexp-quote search-text) content pos))
      (push pos exact-matches)
      (setq pos (1+ pos)))
    (cond
     ;; Exactly one exact match
     ((= (length exact-matches) 1)
      (list :start (car exact-matches)
            :end (+ (car exact-matches) (length search-text))
            :match-type 'exact))
     ;; Multiple exact matches - ambiguous
     ((> (length exact-matches) 1)
      (list :error 'multiple-matches
            :count (length exact-matches)))
     ;; No exact match - try fuzzy
     (t
      (let* ((fuzzy-matches '())
             (normalized-search (gpt--edit-normalize-whitespace search-text))
             (lines (split-string content "\n"))
             (search-line-count (length (split-string search-text "\n"))))
        ;; Slide a window over the content lines
        (cl-loop for i from 0 to (- (length lines) search-line-count)
                 do (let* ((window (cl-subseq lines i (+ i search-line-count)))
                           (window-text (mapconcat #'identity window "\n"))
                           (normalized-window (gpt--edit-normalize-whitespace window-text)))
                      (when (string= normalized-search normalized-window)
                        (push (list :line-start i :line-count search-line-count
                                    :original-text window-text)
                              fuzzy-matches))))
        (cond
         ;; Exactly one fuzzy match
         ((= (length fuzzy-matches) 1)
          (let* ((match (car fuzzy-matches))
                 (line-start (plist-get match :line-start))
                 (before-lines (cl-subseq lines 0 line-start))
                 (start-pos (+ (apply #'+ (mapcar #'length before-lines))
                               line-start)) ; account for newlines
                 (matched-text (plist-get match :original-text)))
            (list :start start-pos
                  :end (+ start-pos (length matched-text))
                  :match-type 'fuzzy
                  :original-text matched-text)))
         ;; Multiple fuzzy matches
         ((> (length fuzzy-matches) 1)
          (list :error 'multiple-matches
                :count (length fuzzy-matches)
                :match-type 'fuzzy))
         ;; No match at all
         (t
          (list :error 'not-found))))))))

(defun gpt--edit-apply-search-replace (content blocks)
  "Apply SEARCH/REPLACE BLOCKS to CONTENT.
Returns a plist with :content (new content) and :errors (list of failed blocks)."
  (let ((result content)
        (errors '())
        (applied 0))
    (dolist (block blocks)
      (let* ((search-text (plist-get block :search))
             (replace-text (plist-get block :replace))
             (match (gpt--edit-find-match search-text result)))
        (cond
         ;; Successful match
         ((and match (not (plist-get match :error)))
          (let ((start (plist-get match :start))
                (end (plist-get match :end)))
            (setq result (concat (substring result 0 start)
                                 replace-text
                                 (substring result end)))
            (setq applied (1+ applied))))
         ;; Error
         (t
          (push (list :block block :error match) errors)))))
    (list :content result
          :errors (nreverse errors)
          :applied applied)))

(defun gpt--edit-format-apply-errors (errors)
  "Format ERRORS from search/replace application for display."
  (mapconcat
   (lambda (err)
     (let* ((block (plist-get err :block))
            (error-info (plist-get err :error))
            (search-text (plist-get block :search))
            (preview (if (> (length search-text) 60)
                         (concat (substring search-text 0 57) "...")
                       search-text)))
       (pcase (plist-get error-info :error)
         ('multiple-matches
          (format "- Multiple matches (%d) for: %s"
                  (plist-get error-info :count)
                  (replace-regexp-in-string "\n" "\\\\n" preview)))
         ('not-found
          (format "- Not found: %s"
                  (replace-regexp-in-string "\n" "\\\\n" preview)))
         (_
          (format "- Unknown error for: %s"
                  (replace-regexp-in-string "\n" "\\\\n" preview))))))
   errors
   "\n"))

(defun gpt--edit-generate-diff (original-text new-content)
  "Generate diff between ORIGINAL-TEXT and NEW-CONTENT.
Returns a plist with :exit-code and :diff-buffer keys.
Exit code is 0 if no changes, 1 if changes exist, or error code on failure."
  (let* ((temp-original (make-temp-file "gpt-edit-original-"))
         (temp-new (make-temp-file "gpt-edit-new-"))
         (diff-program (or (executable-find "diff")
                           (user-error "GPT edit requires the external `diff` program, but it was not found in PATH")))
         (diff-buffer (get-buffer-create "*gpt-edit-diff*"))
         diff-exit)
    (unwind-protect
        (progn
          (with-temp-file temp-original
            (insert original-text))
          (with-temp-file temp-new
            (insert new-content))
          (condition-case err
              (with-current-buffer diff-buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (setq diff-exit
                        (call-process diff-program nil (current-buffer) nil "-u" temp-original temp-new))
                  (unless (memq diff-exit '(0 1))
                    (error "diff exited with status %s" diff-exit))
                  (when (= diff-exit 1)
                    (diff-mode)
                    (goto-char (point-min)))))
            (error
             (message "GPT edit failed while diffing: %s" (error-message-string err))
             (signal (car err) (cdr err))))
          (list :exit-code diff-exit :diff-buffer diff-buffer))
      (delete-file temp-original)
      (delete-file temp-new))))

(defun gpt--edit-apply-changes (source-buffer new-content)
  "Apply NEW-CONTENT to SOURCE-BUFFER, preserving point position.
Creates an undo boundary so changes can be reverted with \\[undo]."
  (with-current-buffer source-buffer
    (let ((inhibit-read-only t)
          (original-point (point)))
      ;; Create an undo boundary before making changes
      (undo-boundary)
      (erase-buffer)
      (insert new-content)
      (goto-char (min original-point (point-max)))
      ;; Create an undo boundary after changes complete
      (undo-boundary))))

(defun gpt--edit-handle-choice (choice source-buffer prompt-buffer new-content
                                       original-text base-command history window-config)
  "Handle user CHOICE (y/n/f) for GPT edit.
Returns t if the edit should be rerun with feedback."
  (pcase choice
    ((or ?y ?Y)
     (when (buffer-live-p prompt-buffer)
       (kill-buffer prompt-buffer))
     (gpt--edit-apply-changes source-buffer new-content)
     (when window-config
       (set-window-configuration window-config))
     (message "Applied GPT edit.")
     nil)
    ((or ?n ?N)
     (when (buffer-live-p prompt-buffer)
       (kill-buffer prompt-buffer))
     (when window-config
       (set-window-configuration window-config))
     (message "Edit rejected.")
     nil)
    (_
     (let* ((feedback (string-trim (read-string "Additional feedback (empty to retry): "))))
       (when (buffer-live-p prompt-buffer)
         (kill-buffer prompt-buffer))
       (let ((updated-history (if (string-empty-p feedback)
                                   history
                                 (cons feedback history))))
         (message "GPT edit: re-running with feedback...")
         (gpt-edit--run source-buffer original-text base-command updated-history window-config))
       t))))

(defun gpt--finalize-edit (source-buffer prompt-buffer original-window original-text initial-size base-command history window-config)
  "Finalize GPT edit and optionally continue with feedback.
SOURCE-BUFFER is the buffer being edited.
PROMPT-BUFFER contains GPT conversation.
ORIGINAL-WINDOW is the window to restore.
ORIGINAL-TEXT is the pre-edit buffer contents.
INITIAL-SIZE marks where GPT output begins.
BASE-COMMAND is the initial instruction.
HISTORY is a list of feedback strings from previous iterations.
WINDOW-CONFIG is the window configuration to restore after editing."
  ;; Restore original window and validate buffers
  (when (window-live-p original-window)
    (select-window original-window))
  (unless (buffer-live-p source-buffer)
    (message "GPT edit aborted: source buffer no longer exists.")
    (cl-return-from gpt--finalize-edit))
  (unless (buffer-live-p prompt-buffer)
    (message "GPT edit aborted: prompt buffer was killed.")
    (cl-return-from gpt--finalize-edit))

  ;; Extract and clean GPT output
  (let ((raw-output (with-current-buffer prompt-buffer
                      (buffer-substring-no-properties initial-size (point-max)))))
    (setq raw-output (gpt--clean-edit-output raw-output))
    (message "GPT edit: proposal captured from %s (%d chars)."
             (buffer-name prompt-buffer)
             (length raw-output))
    (when (string-empty-p (string-trim raw-output))
      (message "GPT edit returned no content.")
      (cl-return-from gpt--finalize-edit))

    ;; Detect format: search/replace blocks or full file replacement
    (let* ((has-search-replace (gpt--edit-has-search-replace-blocks raw-output))
           new-content
           apply-errors
           reran)

      (if has-search-replace
          ;; Search/replace mode
          (let* ((blocks (gpt--edit-parse-search-replace-blocks raw-output))
                 (result (gpt--edit-apply-search-replace original-text blocks))
                 (errors (plist-get result :errors))
                 (applied (plist-get result :applied)))
            (setq new-content (plist-get result :content))
            (setq apply-errors errors)
            (message "GPT edit: parsed %d search/replace blocks, applied %d."
                     (length blocks) applied)
            (when errors
              (message "GPT edit: %d blocks failed to apply." (length errors))))

        ;; Full file replacement mode
        (let* ((extracted (gpt--edit-extract-code-block raw-output))
               (content (string-trim (gpt--edit-strip-code-fences extracted))))
          (setq new-content content)
          (message "GPT edit: using full file replacement mode.")))

      ;; Preserve trailing newline if original had one
      (when (and (string-suffix-p "\n" original-text)
                 (not (string-suffix-p "\n" new-content)))
        (setq new-content (concat new-content "\n")))

      ;; Check if content actually changed
      (if (string= original-text new-content)
          (progn
            (message "GPT produced no changes.")
            (when (buffer-live-p prompt-buffer)
              (kill-buffer prompt-buffer))
            (when window-config
              (set-window-configuration window-config)))

        ;; Generate and display diff
        (let* ((diff-result (gpt--edit-generate-diff original-text new-content))
               (diff-exit (plist-get diff-result :exit-code))
               (diff-buffer (plist-get diff-result :diff-buffer)))
          (if (zerop diff-exit)
              ;; No changes in diff
              (progn
                (when (buffer-live-p diff-buffer)
                  (kill-buffer diff-buffer))
                (when window-config
                  (set-window-configuration window-config))
                (message "GPT edit: diff produced no changes."))

            ;; Show errors if any blocks failed
            (when apply-errors
              (with-current-buffer diff-buffer
                (goto-char (point-max))
                (insert "\n\n--- Search/Replace Errors ---\n"
                        (gpt--edit-format-apply-errors apply-errors)
                        "\n")))

            ;; Display diff and prompt for user action
            (display-buffer diff-buffer)
            (message "GPT edit: review diff and confirm.")
            (let ((choice (read-char-choice
                           "Apply GPT edits, give feedback, or abort (y=yes, f=feedback, n=no)? "
                           '(?y ?Y ?n ?N ?f ?F))))
              (when (buffer-live-p diff-buffer)
                (kill-buffer diff-buffer))
              (setq reran (gpt--edit-handle-choice choice source-buffer prompt-buffer new-content
                                                   original-text base-command history window-config)))))

        ;; Clean up prompt buffer if we didn't rerun
        (unless reran
          (when (buffer-live-p prompt-buffer)
            (kill-buffer prompt-buffer)))))))

(defun gpt--build-edit-command (base-command history)
  "Combine BASE-COMMAND with HISTORY of feedback for a new instruction."
  (let ((chunks (list base-command)))
    (when history
      (setq chunks
            (append chunks
                    (list (concat "Follow-up feedback to incorporate:\n"
                                  (mapconcat (lambda (fb) (concat "- " fb))
                                             (reverse history) "\n"))))))
    (string-join chunks "\n\n")))

(defun gpt-edit--run (source-buffer original-text base-command history &optional window-config)
  "Execute GPT edit iteration for SOURCE-BUFFER.
ORIGINAL-TEXT is the baseline content.
BASE-COMMAND is the initial instruction string.
HISTORY is a list of feedback strings applied so far.
WINDOW-CONFIG is the window configuration to restore after editing (optional)."
  (let* ((effective-command (gpt--build-edit-command base-command history))
         (buffer-title (buffer-name source-buffer))
         (language (with-current-buffer source-buffer (gpt--mode-language major-mode)))
         (file-path (with-current-buffer source-buffer (or (buffer-file-name) "N/A")))
         (selection (with-current-buffer source-buffer
                      (when (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end)))))
         (selection-block
          (when (and selection (not (string-empty-p (string-trim selection))))
            (concat "\nSelected region (for reference only):\n<selection>\n"
                    selection
                    "\n</selection>\n")))
         (prompt (concat
                  "User:\n\n"
                  (format
                   "You are editing the Emacs buffer \"%s\" (File: %s).\n\n"
                   buffer-title file-path)
                  "Choose ONE of these response formats based on the scope of changes:\n\n"
                  "**For small, targeted changes** (preferred when possible): Use SEARCH/REPLACE blocks:\n"
                  "```\n"
                  "<<<<<<< SEARCH\n"
                  "exact code to find\n"
                  "=======\n"
                  "replacement code\n"
                  ">>>>>>> REPLACE\n"
                  "```\n"
                  "You can include multiple SEARCH/REPLACE blocks. The SEARCH text must match exactly (including whitespace).\n\n"
                  (format
                   "**For extensive changes**: Return the complete updated buffer in a ```%s``` code block.\n\n"
                   language)
                  "Important: Do NOT mix formats. Use either SEARCH/REPLACE blocks OR a full file replacement, not both.\n\n"
                  "<instruction>\n"
                  effective-command
                  "\n</instruction>\n"
                  (or selection-block "")
                  "\n<buffer>\n"
                  original-text
                  "\n</buffer>\n"))
         (prompt-buffer (gpt-create-output-buffer (format "Edit %s" buffer-title)))
         (original-window (selected-window)))
    (with-current-buffer prompt-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert prompt)))
    (let ((initial-size (with-current-buffer prompt-buffer (point-max))))
      (switch-to-buffer-other-window prompt-buffer)
      (message "GPT edit: generating proposal...")
      ;; gpt-run-buffer returns the process when using curl
      (let ((proc (gpt-run-buffer prompt-buffer)))
        (unless proc
          (user-error "GPT edit requires curl for streaming. Check that curl is installed and `gpt-use-curl' is t"))
        (let ((original-sentinel (process-sentinel proc))
              (finalized nil))
          (set-process-sentinel
           proc
           (lambda (process event)
             (funcall original-sentinel process event)
             (when (and (not finalized)
                        (memq (process-status process) '(exit signal)))
               (setq finalized t)
               (run-at-time
                0 nil
                (lambda ()
                  (let ((status (process-status process))
                        (exit-code (process-exit-status process)))
                    (if (and (eq status 'exit) (zerop exit-code))
                        (gpt--finalize-edit source-buffer prompt-buffer original-window original-text initial-size base-command history window-config)
                      (when (window-live-p original-window)
                        (select-window original-window))
                      (when window-config
                        (set-window-configuration window-config))
                      (message "GPT edit process exited abnormally (%s, code %s)."
                               (if (eq status 'signal) "signal" "exit")
                               exit-code))))))))
          (message "GPT edit running… diff will appear when ready."))))))

(defun gpt-get-context (context-mode)
  "Return text context based on CONTEXT-MODE.
Always insert <cursor/> in the current buffer.
Possible CONTEXT-MODE values:
- \='all-buffers: all visible buffers plus a <cursor/> in current one
- \='current-buffer: only the current buffer, with a <cursor/>
- nil: no buffer context
If there is an active region, append as \"Selected region:\"."
  (let* ((has-region (use-region-p))
         (region-text (when has-region
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (buffer-content
          (cond
           ((eq context-mode 'all-buffers)
            (let ((current (current-buffer))
                  (visible-buffers (mapcar #'window-buffer (window-list))))
              (mapconcat
               (lambda (buf)
                 (if (eq buf current)
                     (gpt-get-buffer-content-with-cursor buf t)
                   (gpt-get-buffer-content buf t)))
               visible-buffers
               "\n\n")))
           ((eq context-mode 'current-buffer)
            (gpt-get-buffer-content-with-cursor (current-buffer) t))
           (t ""))))
    (cond
     ((and (not (string-empty-p buffer-content)) has-region)
      (format "Buffers (cursor is at <cursor/>):\n\n```\n%s\n```\n\nSelected region:\n\n```\n%s\n```"
              buffer-content region-text))
     ((not (string-empty-p buffer-content))
      (format "Buffers (cursor is at <cursor/>):\n\n```\n%s\n```" buffer-content))
     (has-region
      (format "Selected region:\n\n```\n%s\n```" region-text))
     (t ""))))

(defun gpt-get-output-buffer-name (command)
  "Get the output buffer name for a given COMMAND."
  (let* ((truncated-command (substring command 0 (min gpt-buffer-name-length (length command))))
         (ellipsis (if (< (length truncated-command) (length command)) "..." "")))
    (concat "*gpt"
            "[" (number-to-string gpt-buffer-counter) "]: "
            truncated-command
            ellipsis
            "*")))

(defun gpt-create-output-buffer (command)
  "Create a buffer to capture the output of the GPT process for COMMAND.
If `gpt-use-named-buffers' is non-nil, create or get a named buffer.
Otherwise, create a temporary buffer.  Use the `gpt-mode' for the output buffer."
  (let ((output-buffer
         (if gpt-use-named-buffers
             (let ((buffer (get-buffer-create (gpt-get-output-buffer-name command))))
               (setq gpt-buffer-counter (1+ gpt-buffer-counter))  ; Increment the counter
               buffer)
           (generate-new-buffer (gpt-get-output-buffer-name command)))))
    (with-current-buffer output-buffer
      (gpt-mode))
    output-buffer))

(defun gpt--read-multiple-models ()
  "Return a list of model names selected via `completing-read-multiple'."
  (let* ((choices (mapcar #'car gpt-available-models))
         (prompt "Choose models (comma to separate, finish with RET): "))
    (completing-read-multiple prompt choices nil t)))

(defun gpt--model-id-for-name (name)
  "Return plist for model NAME from `gpt-available-models'.
The plist contains :api, :id, and :max-tokens keys."
  (cdr (assoc name gpt-available-models)))

(defun gpt--format-command-with-model (command model-id)
  "Prefix COMMAND with [MODEL-ID] for buffer naming."
  (format "[%s] %s" model-id command))

;;;###autoload
(defun gpt-chat-multi-models (&optional context-mode prompt-for-models)
  "Run GPT command against multiple models in parallel.

CONTEXT-MODE can be one of:
- \\='all-buffers: Use all visible buffers as context
- \\='current-buffer: Use current buffer as context
- nil or \\='none: Use no buffer context

By default, uses `gpt-multi-models-default' without prompting.
With a prefix argument (C-u), prompts to choose models interactively.

Example usage:
  M-x gpt-chat-multi-models RET current-buffer RET
  C-u M-x gpt-chat-multi-models    ; Interactive model selection
  (gpt-chat-multi-models \\='current-buffer)  ; From Lisp

The command will create separate output buffers for each model,
allowing you to compare responses side-by-side."
  (interactive (let ((choice (completing-read "Context mode: "
                                             '("all-buffers" "current-buffer" "none")
                                             nil t)))
                 (list (unless (string= choice "none") (intern choice))
                       current-prefix-arg)))
  ;; Note: Multi-model validates each API key as it runs each model
  (let* ((command (gpt-read-command context-mode t))
         (models (if prompt-for-models
                     (gpt--read-multiple-models)
                   gpt-multi-models-default)))
    (when (string-empty-p (string-trim command))
      (user-error "Command cannot be empty"))
    (when (null models)
      (user-error "No models selected"))
    ;; Add to history once
    (unless (string-empty-p command)
      (add-to-list 'gpt-command-history command))
    (let* ((input (gpt-get-context context-mode))
           (first t))
      (dolist (model-name models)
        (let* ((info (gpt--model-id-for-name model-name)))
          (unless info
            (user-error "Unknown model: %s" model-name))
          (let* ((model-id (plist-get info :id))
                 (model-max-tokens (or (plist-get info :max-tokens) 64000))
                 (model-thinking-budget (/ model-max-tokens gpt-thinking-budget-fraction))
                 ;; Dynamically bind per-run model and settings
                 ;; API type is derived from gpt-model via gpt--current-api-type
                 (gpt-model model-id)
                 (gpt-max-tokens model-max-tokens)
                 (gpt-thinking-budget model-thinking-budget))
            (let* ((bufname (gpt--format-command-with-model command model-id))
                   (output-buffer (gpt-create-output-buffer bufname)))
              (if first
                  (progn
                    (setq first nil)
                    (switch-to-buffer-other-window output-buffer))
                ;; Ensure each additional model gets its own window
                (display-buffer output-buffer '(display-buffer-pop-up-window)))
              (when (not (string-empty-p input))
                (with-current-buffer output-buffer
                  (insert (format "User:\n\n%s\n\n" input))))
              (with-current-buffer output-buffer
                (gpt-insert-command command)
                (gpt-run-buffer output-buffer)))))))))

(defun gpt-insert-command (command)
  "Insert COMMAND into the current buffer in GPT chat format.
The command is formatted as a User message followed by an Assistant
prompt marker, ready for GPT to generate a response."
  (let ((template "User: %s\n\nAssistant: "))
    (insert (format template command))))

;;;###autoload
(defun gpt-edit-current-buffer ()
  "Rewrite the current buffer via GPT and apply changes after review."
  (interactive)
  (gpt-validate-api-key)
  (let* ((raw-command (gpt-read-command 'current-buffer t))
         (command (string-trim raw-command)))
    (when (string-empty-p command)
      (user-error "Command cannot be empty"))
    (add-to-list 'gpt-command-history raw-command)
    (let ((source-buffer (current-buffer))
          (original-text (buffer-substring-no-properties (point-min) (point-max)))
          (window-config (current-window-configuration)))
      (gpt-edit--run source-buffer original-text command '() window-config))))

;;;###autoload
(defun gpt-chat (&optional context-mode)
  "Run user-provided GPT command with configurable context and print output stream.
CONTEXT-MODE can be:
- \='all-buffers: Use all visible buffers as context
- \='current-buffer: Use current buffer as context
- nil or \='none: Use no buffer context
In all cases, if there is an active region, it will be included."
  (interactive (list (let ((choice (completing-read "Context mode: "
                                                  '("all-buffers" "current-buffer" "none")
                                                  nil t)))
                      (unless (string= choice "none")
                        (intern choice)))))
  (gpt-validate-api-key)
  (let* ((command (gpt-read-command context-mode t))  ; Pass t to use selection
         (output-buffer (gpt-create-output-buffer command))
         (input (gpt-get-context context-mode)))
    ;; Validate command is not empty
    (when (string-empty-p (string-trim command))
      (user-error "Command cannot be empty"))
    ;; Add command to history
    (unless (string-empty-p command)
      (add-to-list 'gpt-command-history command))
    (switch-to-buffer-other-window output-buffer)
    (when (not (string-empty-p input))
      (insert (format "User:\n\n%s\n\n" input)))
    (gpt-insert-command command)
    (gpt-run-buffer output-buffer)))

;;;###autoload
(defun gpt-chat-all-buffers ()
  "Run GPT command with all visible buffers as context."
  (interactive)
  (gpt-chat 'all-buffers))

;;;###autoload
(defun gpt-chat-current-buffer ()
  "Run GPT command with current buffer as context."
  (interactive)
  (gpt-chat 'current-buffer))

;;;###autoload
(defun gpt-chat-no-context ()
  "Run GPT command with no buffer context."
  (interactive)
  (gpt-chat nil))

(defun gpt-follow-up ()
  "Continue the conversation in the current GPT buffer with a new command.
Prompts for a new command, appends it to the buffer in chat format,
and runs GPT to generate a response.  The conversation history in the
buffer provides context for the follow-up.  Must be called from a
buffer in `gpt-mode'."
  (interactive)
  (unless (eq major-mode 'gpt-mode)
    (user-error "Not in a gpt output buffer"))
  (let ((command (gpt-read-command nil nil)))
    (goto-char (point-max))
    (insert "\n\n")
    (gpt-insert-command command)
    (gpt-run-buffer (current-buffer))))

(defun gpt-chat-completion (&optional context-mode)
  "Complete text from cursor position using GPT with configurable context.
CONTEXT-MODE can be:
- \='all-buffers: Use all visible buffers as context
- \='current-buffer: Use current buffer as context
- nil or \='none: Use no buffer context"
  (interactive (list (let ((choice (completing-read "Context mode: "
                                                  '("all-buffers" "current-buffer" "none")
                                                  nil t)))
                      (unless (string= choice "none")
                        (intern choice)))))
  (gpt-validate-api-key)
  (let* ((full-context (gpt-get-context context-mode))
         (source-buffer-name (buffer-name))
         (output-buffer (gpt-create-output-buffer
                        (format "Complete %s" source-buffer-name)))
         (instruction "Continue writing from the <cursor/> position. Match the style and format of the existing text. Provide as ``` code block with language indicator. No meta commentary."))
    ;; Switch to output buffer
    (switch-to-buffer-other-window output-buffer)
    ;; Insert context and instruction with tags
    (insert "User:\n\n"
            "<context>\n"
            full-context
            "\n</context>\n\n"
            "<instruction>\n"
            instruction
            "\n</instruction>\n\n")
    (gpt-insert-command instruction)
    ;; Run GPT
    (gpt-run-buffer output-buffer)))

;;;###autoload
(defun gpt-toggle-thinking ()
  "Toggle extended thinking mode for Anthropic models (enabled by default)."
  (interactive)
  (setq gpt-thinking-enabled (not gpt-thinking-enabled))
  (when (eq (gpt--current-api-type) 'anthropic)
    (gpt-update-backend))
  (message "Extended thinking mode %s"
           (if gpt-thinking-enabled "enabled" "disabled")))

;;;###autoload
(defun gpt-toggle-interleaved-thinking ()
  "Toggle interleaved thinking mode for Anthropic models (enabled by default)."
  (interactive)
  (setq gpt-interleaved-thinking (not gpt-interleaved-thinking))
  (when (eq (gpt--current-api-type) 'anthropic)
    (gpt-update-backend))
  (message "Interleaved thinking mode %s"
           (if gpt-interleaved-thinking "enabled" "disabled")))

;;;###autoload
(defun gpt-toggle-web-search ()
  "Toggle web search for Anthropic models."
  (interactive)
  (setq gpt-web-search (not gpt-web-search))
  (when (eq (gpt--current-api-type) 'anthropic)
    (gpt-update-backend))
  (message "Web search %s"
           (if gpt-web-search "enabled" "disabled")))

;;;###autoload
(defun gpt-thinking-status ()
  "Display current thinking mode settings."
  (interactive)
  (message "Thinking: %s | Interleaved: %s | Web Search: %s | Budget: %s tokens"
           (if gpt-thinking-enabled "ON" "OFF")
           (if gpt-interleaved-thinking "ON" "OFF")
           (if gpt-web-search "ON" "OFF")
           gpt-thinking-budget))

(provide 'gpt-ui)
;;; gpt-ui.el ends here
