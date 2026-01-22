;;; -*- lexical-binding: t; -*-

(use-package python
  :ensure nil
  :defer t
  :init
  (setq doom-modeline-env-python-executable "python3")
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  (setq python-shell-dedicated 'buffer)
  (setq gud-pdb-command-name "python3 -m pdb")
  :hook
  (python-mode . k|lsp-ensure)
  (inferior-python-mode . (lambda () (comint-use-persistent-input-history "~/.python_history")))
  :bind (:map python-mode-map
              ("C-c C-p" . run-python)
              ("C-c C-c" . python-shell-send-buffer)
              ("C-x C-e" . python-shell-send-statement))
  :config

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode) "ty" "server")))

  ;; (add-hook 'inferior-python-mode-hook
  ;;           (lambda ()
  ;;             (company-mode t)))
  (when (eq prelude-lsp-client 'lsp-mode)
    (use-package lsp-pyright)))

;; Integrate with uv
(use-package uv-mode
  :defer t
  :hook (python-mode . uv-mode-auto-activate-hook))

;; (use-package jupyter :defer t)

(provide 'prelude-lang-python)
