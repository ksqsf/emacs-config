;;; -*- lexical-binding: t; -*-

(defvar +dumped-load-path nil
  "Not nil when using dump.")

(load (expand-file-name "custom.el" user-emacs-directory))

(when +dumped-load-path
  (setq load-path +dumped-load-path)
  (setq warning-minimum-level :emergency)
  (global-font-lock-mode t)
  (transient-mark-mode t)
  (message "Skipped startup"))

(unless +dumped-load-path
  (load (expand-file-name "prelude-startup.el" user-emacs-directory)))

(provide 'init)
