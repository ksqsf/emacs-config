;;; -*- lexical-binding: t; -*-
;;; Rust

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-lsp-client k|lsp
        rustic-lsp-server 'rust-analyzer
        rustic-analyzer-command '("~/.local/bin/rust-analyzer"))
  (define-key rustic-mode-map (kbd "[") (k|double-tap-to-insert ?\())
  (define-key rustic-mode-map (kbd "]") (k|double-tap-to-insert ?\)))
  (add-hook 'rustic-mode-hook #'company-mode)
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

(provide 'prelude-lang-rust)
