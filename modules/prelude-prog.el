;;; -*- lexical-binding: t; -*-
;;; Configuration for programming needs.
;;; Some portions might be a standalone module.
;;;
;;; Note, all facilities here are not enabled by default.
;;; Opt-in enable in the corresponding lang-xxx.el file.
;;;
;;; Most of the time, things like company will only be a burden...

(setq-default indent-tabs-mode nil)
(show-paren-mode t)
(setq show-paren-delay 0)

(use-package company
  :commands (company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package imenu
  :bind ("C-c i" . imenu))

(use-package yasnippet
  :commands (yas-minor-mode yas-global-mode)
  :config
  (yas-reload-all))

(use-package hl-todo
  :commands (hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :commands (smartparens-mode))


(use-package lsp-mode
  :commands (lsp lsp-mode))

(use-package lsp-ui
  :commands (lsp-ui-mode))


(use-package dap-mode
  :commands (dap-mode)
  :config
  (require 'dap-lldb))


(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


(defun prelude--enable-prog-features (&rest features)
  (dolist (feature features)
    (pcase feature
      (`:company (company-mode))
      (`:yasnippet (yas-minor-mode))
      (`:hl-todo (hl-todo-mode))
      (`:smartparens (smartparens-mode)))))


;;; GDB
;; Refer to https://debbugs.gnu.org/cgi/bugreport.cgi?bug=33548
(setq gdb-mi-decode-strings 'utf-8)


(provide 'prelude-prog)
