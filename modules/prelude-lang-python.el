;;; -*- lexical-binding: t; -*-

(use-package python
  :defer t
  :init
  (setq doom-modeline-env-python-executable "python3")
  (setq python-shell-interpreter "python3")
  (setq gud-pdb-command-name "python3 -m pdb")
  (setq python-shell-interpreter-args "-i")
  :bind (:map python-mode-map
              ("C-x C-e" . python-shell-send-statement))
  :config
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (company-mode t))))

(use-package elpy
  :disabled
  :after (python)
  :hook (python-mode . elpy-enable)
  :config
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq elpy-rpc-python-command "python3"
        elpy-rpc-virtualenv-path (no-littering-expand-var-file-name "elpy/rpc-venv")
        elpy-get-info-from-shell t))

(use-package flycheck-mypy
  :disabled
  :after python
  :config
  (setq flycheck-python-mypy-args "--no-incremental"))

(use-package lsp-pyright
  :hook (python-mode . lsp))

(provide 'prelude-lang-python)
