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
(setq show-paren-delay 0.0)
(setq tab-always-indent 'complete)

;; (use-package company
;;   :commands (company-mode)
;;   :bind (:map company-active-map
;;               (("<tab>" . company-complete-selection)
;;                ("<return>" . nil)
;;                ("RET" . nil)))
;;   :config
;;   (setq company-idle-delay 0.15))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   ;; WARNING: Don't use all-the-icons!!
;;   (setq company-box-icons-alist 'company-box-icons-idea))

(use-package yasnippet
  :commands (yas-minor-mode yas-global-mode)
  :config
  (yas-reload-all))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :commands (hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :commands (smartparens-mode))


(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-confirm-server-initiated-edits nil
        eglot-autoreconnect 60
        eglot-autoshutdown t))

;;
;; lsp-mode is powerful and cool!  but it has severe performance
;; problems. not lsp-mode but emacs itself is to blame.
;;
(use-package lsp-mode
  :commands (lsp lsp-mode)
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-enable-snippet t
        lsp-lens-enable nil)

  ;; *DANGER*: It warns that you shouldn't use this unless you know
  ;; the internals of lsp-mode.
  ;; その時はその時だ
  (setq lsp-auto-guess-root t))

(use-package lsp-ui
  :commands (lsp-ui-mode))

(defvar k|lsp 'eglot)  ;; lsp or eglot

(defun k|lsp-ensure ()
  (if (eq k|lsp 'lsp)
      (lsp)
    (eglot-ensure)))


(use-package dap-mode
  :commands (dap-mode)
  :config
  (require 'dap-lldb))


(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;;; GDB
;; Refer to https://debbugs.gnu.org/cgi/bugreport.cgi?bug=33548
(setq gdb-mi-decode-strings 'utf-8)


(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative
      display-line-numbers-grow-only t)


(provide 'prelude-prog)
