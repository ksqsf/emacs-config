(defvar k|emacs-uptime-log-file
  (expand-file-name "var/uptime.org" user-emacs-directory))

(defun k|emacs-record-uptime ()
  (with-temp-buffer
    (insert
     "|"
     (format-time-string "%FT%T%z" before-init-time)
     " | "
     (format-time-string "%FT%T%z" (current-time))
     " | "
     (emacs-uptime)
     " |"
     "\n")
    (append-to-file nil nil k|emacs-uptime-log-file)))

(add-hook 'kill-emacs-hook 'k|emacs-record-uptime)
