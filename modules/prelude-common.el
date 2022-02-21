;;; -*- lexical-binding: t; -*-
;; Supporting functions that every module might want to use.
;; NOTE: This file should NOT depend on any third-party packages.

(defmacro k|with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the
*Messages* buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defconst k|mac (eq system-type 'darwin))

(defmacro k|double-tap-to-insert (to-char)
  "Create a function suitable for key binding.  It replaces
double consecutive occurrences of that character with TO-CHAR."
  `(lambda (cnt raw)
     (interactive "p\nP")
     (if (and (eq (preceding-char) last-command-event)
              (not raw))
         (progn
           (backward-delete-char 1)
           (insert ,to-char))
       (self-insert-command cnt))))

(defconst k|default-opener
  "The default file opener on the current system. (No Windows support.)"
  (if k|mac
      "open"
    "xdg-open"))

(require 'cl-lib)

(provide 'prelude-common)

