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

(use-package lsp-pyright
  :hook (python-mode . lsp))

(provide 'prelude-lang-python)
