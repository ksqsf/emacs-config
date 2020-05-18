;;; -*- lexical-binding: t; -*-
;; Supporting functions that every module might want to use.
;; NOTE: This file should NOT depend on any third-party packages.

(defgroup prelude
  nil
  "Prelude configuration group.")

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the
  `*Messages*` buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defconst *is-a-mac* (eq system-type 'darwin))

(defmacro double-tap-to-insert (to-char)
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

(defun require-async (feature)
  "Call `require' in a new thread.

The effect is yet to be experimented."
  (make-thread
   (lambda ()
     (require feature))))

(setq user-full-name "ksqsf"
      user-mail-address "i@ksqsf.moe")

(provide 'prelude)
