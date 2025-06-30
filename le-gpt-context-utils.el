;;; le-gpt-context-utils.el --- Utility functions for le-gpt context -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Shared utility functions used across context modules.

;;; Code:

(defun le-gpt--format-file-size (bytes)
  "Format BYTES as human readable size."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fK" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fM" (/ bytes 1024.0 1024.0)))
   (t (format "%.1fG" (/ bytes 1024.0 1024.0 1024.0)))))

(defun le-gpt--format-time-ago (time)
  "Format TIME as relative time ago."
  (if (not time)
      ""
    (let* ((now (current-time))
           (diff (time-subtract now time))
           (seconds (time-to-seconds diff)))
      (cond
       ((< seconds 60) "now")
       ((< seconds 3600) (format "%dm" (/ seconds 60)))
       ((< seconds 86400) (format "%dh" (/ seconds 3600)))
       ((< seconds 604800) (format "%dd" (/ seconds 86400)))
       (t (format "%dw" (/ seconds 604800)))))))

(provide 'le-gpt-context-utils)
;;; le-gpt-context-utils.el ends here
