;;; -*- lexical-binding: t; -*-

;; talk with os-level utilities

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(defun open-directory-here ()
  "Open `default-directory'  with `system-opener'.

In most cases, this means the current directory of the current buffer."
  (interactive)
  (k|with-suppressed-message
    (shell-command (format "%s %s" k|default-opener (shell-quote-argument (expand-file-name default-directory))))))

(global-set-key (kbd "C-c d") #'open-directory-here)

(provide 'prelude-os)
