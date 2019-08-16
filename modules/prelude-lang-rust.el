;;; Rust

(ensure-package 'rust-mode)
(ensure-package 'cargo)
(ensure-package 'racer)

(defun prelude--setup-rust-mode ()
  (yas-minor-mode t)
  (cargo-minor-mode t)
  (racer-mode t)
  (subword-mode t))

(add-hook 'rust-mode-hook #'prelude--setup-rust-mode)
(add-hook 'find-file-hook
	  (lambda ()
	    (when (string= (file-name-nondirectory buffer-file-name) "Cargo.toml")
	      (cargo-minor-mode))))

(provide 'prelude-lang-rust)
