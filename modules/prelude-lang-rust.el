;;; -*- lexical-binding: t; -*-
;;; Rust

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-lsp-client k|lsp
        rustic-lsp-server 'rust-analyzer
        rustic-analyzer-command '("rust-analyzer")
        rustic-enable-detached-file-support t)
  (define-key rustic-mode-map (kbd "[") (k|double-tap-to-insert ?\())
  (define-key rustic-mode-map (kbd "]") (k|double-tap-to-insert ?\)))
  ;; (add-hook 'rustic-mode-hook #'company-mode)
  (add-hook 'rustic-mode-hook #'subword-mode)
  (add-hook 'rustic-mode-hook #'electric-pair-mode)

  ;; fix a bug
  (defun rustic-cargo-doc ()
    "Open the documentation for the current project in a browser.
The documentation is built if necessary."
    (interactive)
    (if (y-or-n-p "Open docs for dependencies as well?")
        (shell-command (concat (rustic-cargo-bin) " doc --open"))
      (shell-command (concat (rustic-cargo-bin) " doc --open --no-deps")))))

(use-package ob-rust
  :after org)

(defun cargo-play (arg)
  (interactive "P")
  (let* ((release-flag (if arg "--release" ""))
         (command (format "cargo play %s %s &" release-flag (buffer-file-name))))
    (shell-command command "*Cargo Play*")))

(defun cargo (package-name)
  "Create a new cargo package under /tmp."
  (interactive "sPackage name: ")
  (let ((old-pwd default-directory))
    (cd "/tmp")
    (shell-command (concat "cargo new " package-name))
    (cd old-pwd))
  (find-file (concat "/tmp/" package-name "/src/main.rs")))

(defun rust-show (flags mode)
  (let* ((command (format "rustc %s -o /tmp/emacs-output %s" (mapconcat #'identity flags " ") (buffer-file-name)))
         (buffer (get-buffer-create "*Rust Show*")))
    (shell-command command " *Rust Show Output*")
    (switch-to-buffer-other-window buffer)
    (insert-file "/tmp/emacs-output")
    (funcall mode)))

(defun rust-show-mir (arg)
  (interactive "P")
  (let* ((opt-flag (if arg "-O" "")))
    (rust-show (list opt-flag "--emit=mir") #'fundamental-mode)))

(defun rust-show-asm (arg)
  (interactive "P")
  (let* ((opt-flag (if arg "-O" "")))
    (rust-show (list opt-flag "--emit=asm") #'asm-mode)))

(defun rust-show-llvm-ir (arg)
  (interactive "P")
  (let* ((opt-flag (if arg "-O" "")))
    (rust-show (list opt-flag "--emit=llvm-ir") #'fundamental-mode)))

(defun rust (arg)
  (interactive "P")
  (find-file "/tmp/play.rs"))

(use-package pest-mode
  :load-path "lisp/pest-mode"
  :mode ("\\.pest\\'" . pest-mode))

(provide 'prelude-lang-rust)
