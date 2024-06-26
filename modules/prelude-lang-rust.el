;;; -*- lexical-binding: t; -*-
;;; Rust

(use-package rust-ts-mode
  :hook (rust-ts-mode . k|lsp-ensure)
  :hook (rust-ts-mode . subword-mode)
  :hook (rust-ts-mode . electric-pair-mode)
  :hook (rust-ts-mode . cargo-minor-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

;; support rustc output
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'rustc)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(rustc . ("\\(?:\\(?:error\\)\\|\\(warning\\)\\).*?: .*
 +--> \\(.*?\\):\\([0-9]+\\):\\([0-9]+\\)" 2 3 4 (1)))))

(use-package cargo
  :defer t)

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
    (insert-file-contents "/tmp/emacs-output")
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

(defun rust (_arg)
  (interactive "P")
  (find-file "/tmp/play.rs"))

(use-package pest-mode
  :load-path "lisp/pest-mode"
  :mode ("\\.pest\\'" . pest-mode))

(provide 'prelude-lang-rust)
