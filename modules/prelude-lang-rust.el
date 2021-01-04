;;; -*- lexical-binding: t; -*-
;;; Rust

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot
        rustic-lsp-server 'rust-analyzer
        rustic-analyzer-command '("~/.local/bin/rust-analyzer"))
  (define-key rustic-mode-map (kbd "[") (double-tap-to-insert ?\())
  (define-key rustic-mode-map (kbd "]") (double-tap-to-insert ?\)))
  (add-hook 'rustic-mode-hook #'yas-minor-mode)
  (add-hook 'rustic-mode-hook #'subword-mode)
  (add-hook 'rustic-mode-hook #'electric-pair-mode))

(use-package ob-rust
  :after org)

(defun cargo-play (arg)
  (interactive "P")
  (let* ((release-flag (if arg "--release" ""))
         (command (format "cargo play %s %s &" release-flag current-file)))
    (shell-command command "*Cargo Play*")))

(provide 'prelude-lang-rust)
