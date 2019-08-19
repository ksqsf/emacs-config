;;; -*- lexical-binding: t; -*-
;;; Rust

(ensure-package 'rust-mode)
(ensure-package 'cargo)
(ensure-package 'racer)

(defun prelude/enter-rust ()
  (yas-minor-mode t)
  (cargo-minor-mode t)
  (racer-mode t)
  (subword-mode t)

  ;; Racer is very slow
  (setq-local company-idle-delay nil))

(add-hook 'rust-mode-hook #'prelude/enter-rust)
(add-hook 'find-file-hook
          (lambda ()
            (when (string= (file-name-nondirectory buffer-file-name) "Cargo.toml")
              (cargo-minor-mode))))

(provide 'prelude-lang-rust)
