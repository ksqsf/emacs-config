;;; Rust

(ensure-package 'rust-mode)
(ensure-package 'cargo)
(ensure-package 'racer)

(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'find-file-hook
	  (lambda ()
	    (when (string= (file-name-nondirectory buffer-file-name) "Cargo.toml")
	      (cargo-minor-mode))))
(add-hook 'rust-mode-hook #'racer-mode)

(provide 'prelude-lang-rust)
