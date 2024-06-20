;;; -*- lexical-binding: t; -*-

(use-package python
  :ensure nil
  :defer t
  :init
  (setq doom-modeline-env-python-executable "python3")
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "--simple-prompt")
  (setq python-shell-dedicated t)
  (setq gud-pdb-command-name "python3 -m pdb")
  :hook
  (python-mode . k|lsp-ensure)
  (inferior-python-mode . (lambda () (comint-use-persistent-input-history "~/.python_history")))
  :bind (:map python-mode-map
              ("C-c C-p" . run-python)
              ("C-c C-c" . python-shell-send-buffer)
              ("C-x C-e" . python-shell-send-statement))
  :config
  ;; (add-hook 'inferior-python-mode-hook
  ;;           (lambda ()
  ;;             (company-mode t)))
  (when (eq prelude-lsp-client 'lsp-mode)
    (use-package lsp-pyright)))

;; (use-package jupyter :defer t)

(provide 'prelude-lang-python)
