;;; -*- lexical-binding: t; -*-

(use-package python
  :ensure nil
  :defer t
  :init
  (setq doom-modeline-env-python-executable "python3")
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "--simple-prompt")
  (setq gud-pdb-command-name "python3 -m pdb")
  :hook
  (python-mode . k|lsp-ensure)
  :bind (:map python-mode-map
              ("C-c C-p" . run-python)
              ("C-c C-c" . python-shell-send-buffer)
              ("C-x C-e" . python-shell-send-statement))
  :config
  ;; (add-hook 'inferior-python-mode-hook
  ;;           (lambda ()
  ;;             (company-mode t)))
  (when (eq k|lsp 'lsp)
    (use-package lsp-pyright)))

(provide 'prelude-lang-python)
