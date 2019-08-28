;;; -*- lexical-binding: t; -*-

;;; straight.el
(defvar straight-check-for-modifications '(check-on-save find-when-checking))
(defvar straight-bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
(defvar straight-bootstrap-version 5)
(unless (file-exists-p straight-bootstrap-file)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(load straight-bootstrap-file nil 'nomessage)

;;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)

(provide 'prelude-straight)
