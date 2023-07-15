;;; -*- lexical-binding: t; -*-
;;; Configuration for programming needs.
;;; Some portions might be a standalone module.

(setq-default indent-tabs-mode nil)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq tab-always-indent 'complete)
(add-hook 'prog-mode-hook 'turn-on-adaptive-wrap)
(setq xref-history-storage 'xref-window-local-history)


(use-package yaml-mode :defer t)
(use-package dockerfile-mode :defer t)



(use-package yasnippet
  :commands (yas-minor-mode yas-global-mode))

(use-package yasnippet-snippets
  :after (yasnippet)
  :defer 10
  :config
  (yasnippet-snippets-initialize))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

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

  (defvar prelude--eglot-enable-debug nil
    "Non-nil when JSON-RPC logging is enabled.

Use `k|toggle-eglot-debug' to change this value.")

  (defun prelude--jsonrpc-ignore-log (_orig-func &rest _))

  (defun k|toggle-eglot-debug ()
    (interactive)
    (if prelude--eglot-enable-debug
        (progn
          (advice-remove 'jsonrpc--log-event 'prelude--jsonrpc-ignore-log)
          (setq prelude--eglot-enable-debug nil))
      (advice-add 'jsonrpc--log-event :around 'prelude--jsonrpc-ignore-log)
      (setq prelude--eglot-enable-debug t)))

  (fset #'eglot--snippet-expansion-fn #'ignore))

;;
;; lsp-mode is powerful and cool!  but it has severe performance
;; problems. not lsp-mode but emacs itself is to blame.
;;
(use-package lsp-mode
  :commands (lsp lsp-mode)
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-enable-snippet t)

  ;; LSP lens performs poorly on older Emacs versions.
  ;; Emacs 29 introduced noverlay -- which make overlays exponentially faster.
  (setq lsp-lens-enable (not (version< emacs-version "29")))

  ;; *DANGER*: It warns that you shouldn't use this unless you know
  ;; the internals of lsp-mode.
  ;; その時はその時だ
  (setq lsp-auto-guess-root t))

(use-package lsp-ui
  :after (lsp-mode)
  :commands (lsp-ui-mode))

(use-package lsp-bridge
  :load-path "lisp/lsp-bridge/"
  :commands (global-lsp-bridge-mode lsp-bridge-mode))

(defcustom k|auto-lsp nil
  "Whether to start lsp automatically on all supported languages."
  :group 'prelude
  :type 'boolean)

(defcustom k|lsp 'eglot
  "The LSP client to use.

One of `lsp-mode', `eglot', or `lsp-bridge'."
  :group 'prelude
  :type 'symbol)

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


(use-package copilot
  :disabled
  :hook (prog-mode . copilot-mode)
  :vc (:fetcher github :repo "zerolfx/copilot.el")
  :bind (("C-<tab>" . 'copilot-accept-completion-by-word)))


(use-package citre
  :hook (c-mode-common . citre-mode))


(use-package quickrun
  :defer t)


(defun comint-use-persistent-input-history (filename)
  "Enable persistent input history in a Comint buffer.
The history is stored in FILENAME."
  (setq comint-input-ring-file-name filename)
  (comint-read-input-ring t))


(use-package symbol-overlay
  :diminish ""
  :hook (prog-mode . symbol-overlay-mode)
  :bind ("C-c i" . symbol-overlay-put))


;; show git diff info in fringe
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode)
  ;; diff-hl-dired-mode is buggy.
  ;; :hook (dired-mode . diff-hl-dired-mode)
  :config
  (diff-hl-margin-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq diff-hl-disable-on-remote t))


;; structural navigation and editing
;; depends on emacs 29 treesit.el
(use-package combobulate
  :disabled
  :defer t
  :vc (:fetcher github :repo "mickeynp/combobulate")
  :preface
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))


(use-package codeium
  :disabled
  :vc (:fetcher github :repo "Exafunction/codeium.el")
  :init
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)))
  ;; (setq codeium-mode-line-enable
  ;;       (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  ;; (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; (setq codeium-api-enabled
  ;;       (lambda (api)
  ;;         (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; (defun my-codeium/document/text ()
  ;;   (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; (defun my-codeium/document/cursor_offset ()
  ;;   (codeium-utf8-byte-length
  ;;    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  ;; (setq codeium/document/text 'my-codeium/document/text)
  ;; (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
  )


(use-package devdocs
  :commands (devdocs-install devdocs-update-all devdocs-search))


(provide 'prelude-prog)
