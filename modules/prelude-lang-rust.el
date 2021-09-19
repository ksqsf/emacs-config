;;; -*- lexical-binding: t; -*-
;;; Rust

(defun rust-compile-command ()
  "Set `compile-command' for rust(ic)-mode buffers."
  (when buffer-file-name
    (setq-local compile-command (concat "rustc "
                                        (shell-quote-argument buffer-file-name)
                                        " -o a.out && ./a.out"))))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . rust-compile-command))
  
  
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . rust-compile-command)
  :config
  (setq rustic-lsp-client 'lsp-mode
        rustic-lsp-server 'rust-analyzer
        rustic-analyzer-command '("~/.local/bin/rust-analyzer"))
  (define-key rustic-mode-map (kbd "[") (double-tap-to-insert ?\())
  (define-key rustic-mode-map (kbd "]") (double-tap-to-insert ?\)))
  (add-hook 'rustic-mode-hook #'company-mode)
  (add-hook 'rustic-mode-hook #'yas-minor-mode)
  (add-hook 'rustic-mode-hook #'subword-mode)
  (add-hook 'rustic-mode-hook #'electric-pair-mode))

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
