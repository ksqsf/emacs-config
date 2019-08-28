;;; -*- lexical-binding: t; -*-
;;; Configuration for programming needs.
;;; Some portions might be a standalone module.

(setq-default indent-tabs-mode nil)

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.3))

(use-package imenu
  :bind (:map prog-mode-map
	      ("C-c C-j" . imenu)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;; GDB
;; Refer to https://debbugs.gnu.org/cgi/bugreport.cgi?bug=33548
(setq gdb-mi-decode-strings 'utf-8)


(provide 'prelude-prog)
