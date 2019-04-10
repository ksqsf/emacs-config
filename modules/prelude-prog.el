;;; Configuration for programming needs.
;;; Some portions might be a standalone module.

(defun prelude--enter-prog ()
  "Common tasks before entering a `prog-mode'-derived major
mode."
  (hl-todo-mode t)               			    ; Highlight TODOs
  (company-mode t)                                          ; Auto complete
  )

(add-hook 'prog-mode-hook #'prelude--enter-prog)


;;; CC mode
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(c-mode . "k&r")
			(c++-mode . "stroustrup")
			(other . "gnu")))

(defun prelude--enter-cc ()
  (yas-minor-mode)
  (define-key c-mode-map (kbd "<f5>") #'compile)
  (define-key c++-mode-map (kbd "<f5>") #'compile))

(add-hook 'c-mode-common-hook #'prelude--enter-cc)
(add-hook 'c++-mode-hook #'prelude--enter-cc)


;;; GDB
(setq gdb-mi-decode-strings 'utf-8)


;;; Rust
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


;;; OCaml
(unless (package-installed-p 'tuareg)
  (package-install 'tuareg))


;;; Lisp
(unless (package-installed-p 'paredit)
  (package-install 'paredit))

(defun prelude--enter-lisp ()
  (paredit-mode 1))

(add-hook 'lisp-mode-hook #'prelude--enter-lisp)
(add-hook 'emacs-lisp-mode-hook #'prelude--enter-lisp)


;;; Javascript-related
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun run-node ()
  (interactive)
  (setenv "NODE_NO_READLINE" "1")
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))


(provide 'prelude-prog)
