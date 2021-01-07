;;; -*- lexical-binding: t; -*-
;;; Rust

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-lsp-client 'lsp-mode
        rustic-lsp-server 'rust-analyzer
        rustic-analyzer-command '("~/.local/bin/rust-analyzer"))
  (define-key rustic-mode-map (kbd "[") (double-tap-to-insert ?\())
  (define-key rustic-mode-map (kbd "]") (double-tap-to-insert ?\)))
  (add-hook 'rustic-mode-hook #'company-mode)
  (add-hook 'rustic-mode-hook #'yas-minor-mode)
  (add-hook 'rustic-mode-hook #'subword-mode)
  (add-hook 'rustic-mode-hook #'electric-pair-mode))

(use-package ob-rust
  :after org)

(defun cargo-play (arg)
  (interactive "P")
  (let* ((release-flag (if arg "--release" ""))
         (command (format "cargo play %s %s &" release-flag (buffer-file-name))))
    (shell-command command "*Cargo Play*")))

(provide 'prelude-lang-rust)
