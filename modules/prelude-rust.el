;; Cargo
(unless (package-installed-p 'cargo)
  (package-install 'cargo))

(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'find-file-hook
	  (lambda ()
	    (when (string= (file-name-nondirectory buffer-file-name) "Cargo.toml")
	      (cargo-minor-mode))))

;; Racer
(unless (package-installed-p 'racer)
  (package-install 'racer))

(add-hook 'rust-mode-hook #'racer-mode)

(provide 'prelude-rust)
