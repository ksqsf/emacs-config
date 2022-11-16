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
        eglot-autoshutdown t)

  ;; By default, turn off event logging for performance.
  (advice-add 'jsonrpc--log-event :around
              (lambda (_orig-func &rest _)))

  (fset #'eglot--snippet-expansion-fn #'ignore))

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

(use-package lsp-bridge
  :load-path "lisp/lsp-bridge/"
  :commands (lsp-bridge-mode)
  :config
  (require 'lsp-bridge-orderless)
  (require 'lsp-bridge-icon))

(defcustom k|auto-lsp nil
  "Whether to start lsp automatically on all supported languages."
  :group 'prelude)

(defcustom k|lsp 'eglot
  "The LSP client to use.

One of `lsp-mode', `eglot', or `lsp-bridge'."
  :group 'prelude)

(defun k|lsp-ensure ()
  (interactive)
  (catch 'foo
    (cond ((not k|auto-lsp)
           (throw 'foo nil))
          ((eq k|lsp 'lsp-bridge)
           (lsp-bridge-mode)
           ;; Let lsp-bridge control when popups should be displayed.
           (setq corfu-auto nil))
          ((eq k|lsp 'lsp-mode)
           (lsp-deferred))
          ((eq k|lsp 'eglot)
           (eglot-ensure)))))


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


(use-package copilot
  :disabled
  :hook (prog-mode . copilot-mode)
  :quelpa (copilot :fetcher github :repo "zerolfx/copilot.el")
  :bind (("C-<tab>" . 'copilot-accept-completion-by-word)))


(use-package citre
  :hook (c-mode-common . citre-mode))


(use-package quickrun
  :defer t)


(provide 'prelude-prog)
