;;; -*- lexical-binding: t; -*-

(defun system-opener ()
  "Invoke the file opener shipped by the operating system.

It should be able to handle all kinds of files."
  (if *is-a-mac*
      "open"
    "xdg-open"))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(defun open-directory-here ()
  "Open `default-directory'  with `system-opener'.

In most cases, this means the current directory of the current buffer."
  (interactive)
  (with-suppressed-message
    (shell-command (format "%s %s" (system-opener) (shell-quote-argument (expand-file-name default-directory))))))

(global-set-key (kbd "C-c d") #'open-directory-here)

(provide 'prelude-utils)
