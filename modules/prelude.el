;;; -*- lexical-binding: t; -*-
;; Supporting functions that every module might want to use.

(defun ensure-package (package)
  "Ensure that we've got PACKAGE installed.  If it isn't, install
it immediately."
  (unless (package-installed-p package)
    (package-install package)))

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the
  `*Messages*` buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defconst *is-a-mac* (eq system-type 'darwin))

(provide 'prelude)
