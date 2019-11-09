;;; -*- lexical-binding: t; -*-
;;; Rust

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (define-key rust-mode-map (kbd "[") (double-tap-to-insert ?\())
  (define-key rust-mode-map (kbd "]") (double-tap-to-insert ?\)))
  (add-hook 'rust-mode-hook #'yas-minor-mode)
  (add-hook 'rust-mode-hook #'subword-mode)
  (add-hook 'rust-mode-hook #'(lambda () (setq company-idle-delay nil))))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :config
  (add-hook 'find-file-hook
            (lambda ()
              (when (string= (file-name-nondirectory buffer-file-name) "Cargo.toml")
                (cargo-minor-mode)))))

(use-package racer
  :hook (rust-mode . racer-mode))

(use-package ob-rust
  :after org)

(provide 'prelude-lang-rust)
