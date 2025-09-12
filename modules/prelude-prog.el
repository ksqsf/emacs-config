;;; preloude-prog ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Configuration for programming needs.
;;; Some portions might be a standalone module.

;;; Code:

(setq-default indent-tabs-mode nil)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq tab-always-indent 'complete)
(add-hook 'prog-mode-hook 'turn-on-adaptive-wrap)
(setq xref-history-storage 'xref-window-local-history)
(setq xref-search-program 'ripgrep)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)


;;; Tree-sitter basic configuration
(when (boundp 'treesit-extra-load-path)
  (add-to-list 'treesit-extra-load-path (no-littering-expand-var-file-name "tree-sitter")))

;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)   ;; python-ts-mode is better than python-mode at indentation
;;         ))
(use-package yaml-mode
  :iload (yaml-mode)
  :mode ("\\.ya?ml\\'" . yaml-mode))
(use-package dockerfile-mode
  :mode ("Dockerfile.*\\'" . dockerfile-mode)
  :mode (".dockerfile\\'" . dockerfile-mode))

(with-eval-after-load 'treesit
  (defun treesit--explorer-jump-advice-really-jump (button)
    "Really jump to the node, not just highlight it."
    (with-current-buffer treesit--explorer-source-buffer
      (goto-char (button-get button 'node-start))))
  (advice-add 'treesit--explorer-jump :after #'treesit--explorer-jump-advice-really-jump))


;;; Snippets
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
  :hook (c-common-mode . rainbow-delimiters-mode))

(use-package puni
  :disabled
  :hook (prog-mode . puni-mode))


;;; Language Server Protocol
(setq read-process-output-max (* 1024 1024))

(use-package eglot
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-confirm-server-initiated-edits nil
        eglot-autoreconnect 60
        eglot-autoshutdown t)

  (defun toggle-eglot-debug ()
    (interactive)
    (if (= 0 eglot-events-buffer-size)
        (setq eglot-events-buffer-size 20000)
      (setq eglot-events-buffer-size 0)))
  (setq eglot-events-buffer-size 0)

  ;; eglot expansion should be okay these days...
  ;; (fset #'eglot--snippet-expansion-fn #'ignore)

  ;; The buster conflicts with corfu, causing repeated insertions.
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(use-package eglot-booster
  :disabled
  :vc (:fetcher github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

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

(defcustom prelude-auto-lsp nil
  "Whether to start lsp automatically on all supported languages."
  :group 'prelude
  :type 'boolean)

(defcustom prelude-lsp-client 'eglot
  "The LSP client to use.

One of `lsp-mode', `eglot', or `lsp-bridge'."
  :group 'prelude
  :type 'symbol)

(defun k|lsp-ensure ()
  "Ensure the preferred lsp client has started."
  (interactive)
  (catch 'foo
    (cond ((not prelude-auto-lsp)
           (throw 'foo nil))
          ((eq prelude-lsp-client 'lsp-bridge)
           (lsp-bridge-mode)
           ;; Let lsp-bridge control when popups should be displayed.
           (setq corfu-auto nil))
          ((eq prelude-lsp-client 'lsp-mode)
           (lsp-deferred))
          ((eq prelude-lsp-client 'eglot)
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
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("<right>" . copilot-accept-completion)
              ("M-f" . copilot-accept-completion-by-word)
              ("C-e" . copilot-accept-completion-by-line)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion))
  :config
  (setq copilot-network-proxy '(:host "127.0.0.1" :port 7890))
  (setq copilot-balancer-debug-buffer (get-buffer-create " *copilot-balancer*")))


(use-package citre
  :hook (c-mode-common . citre-mode)
  :hook (c-ts-mode . citre-mode)
  :hook (c++-ts-mode . citre-mode)
  :hook (swift-mode . citre-mode))


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
  :defer t
  :vc (:fetcher github :repo "mickeynp/combobulate")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode))
  :bind (:map combobulate-key-map
              (("C-M-U" . combobulate-mark-node-dwim)
               ("M-h" . nil))))


(use-package devdocs
  :commands (devdocs-install devdocs-update-all devdocs-search))


(use-package realgud
  :defer t)


(with-eval-after-load 'ebrowse
  (keymap-unset ebrowse-tree-mode-map "C-l"))


(use-package rmsbolt
  :defer t)

(defun godbolt ()
  "Send the current buffer to Compiler Explorer."
  (interactive)
  (let ((url (goldbolt--construct-url)))
    (browse-url (format "https://godbolt.org/%s" ()))))


(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode))


(use-package cmake-mode)

(use-package eldoc-cmake
  :hook (cmake-mode-hook . eldoc-cmake-enable))


(use-package flymake
  :ensure nil
  :config
  (setopt flymake-show-diagnostics-at-end-of-line 'short))


(use-package indent-bars
  :config
  (setq indent-bars-no-descend-lists t)
  (setq indent-bars-display-on-blank-lines 'least)
  (setq indent-bars-treesit-support t)
  :hook ((prog-mode . indent-bars-mode)
         (lua-mode . (lambda ()
                       (setq indent-bars-spacing-override lua-indent-level)))
         (c-common-mode . (lambda ()
                            (setq-local indent-bars-spacing-override 4)))))


(provide 'prelude-prog)
;;; prelude-prog.el ends here
