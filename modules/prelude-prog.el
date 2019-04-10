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


;;; Resolve Coq and Verilog
(defvar coq-or-verilog-mode--regexp "\\(?:\\(Theorem\\|Ltac\\|Example\\|Lemma\\)[ 	]\\).+:")
(defun coq-or-verilog-mode ()
  "Analyze buffer and enable either Coq or Verilog mode.

Coq uses .v extension for Coq files, which is also the one used
for Verilog files.  This makes matching on file name insufficient
for detecting major mode that should be used.

This function attempts to use file contents to determine whether
the code is Coq or Verilog and based on that chooses whether to
enable `coq-mode' or `verilog-mode'.

The testing procedure and criteria are not sufficiently enough,
but works well enough for distinguishing source files in Software
Foundations and typical Verilog files."
  (if (save-excursion
        (save-restriction
          (save-match-data
            (widen)
            (goto-char (point-min))
            (re-search-forward coq-or-verilog-mode--regexp
                               (point-max) t))))
      (coq-mode)
    (verilog-mode)))
(add-to-list 'auto-mode-alist '("\\.v\\'" . coq-or-verilog-mode))


(provide 'prelude-prog)
