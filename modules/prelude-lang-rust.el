;;; -*- lexical-binding: t; -*-
;;; Rust

(use-package rust-mode
  :hook (rust-mode . k|lsp-ensure)
  :hook (rust-mode . subword-mode)
  :hook (rust-mode . electric-pair-mode)
  :hook (rust-mode . cargo-minor-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

  :config
  ;; Remove syntax-ppss-flush-cache to fix the performance issue with
  ;; adaptive-wrap-prefix-mode.  See
  ;; https://github.com/rust-lang/rust-mode/issues/558
  (defun rust-in-comment-paragraph (body)
    (save-excursion
      (when (not (nth 4 (syntax-ppss)))
        (beginning-of-line)
        (when (looking-at (concat "[[:space:]\n]*" comment-start-skip))
          (goto-char (match-end 0))))

      (let ((next-bol (line-beginning-position 2)))
        (while (save-excursion
                 (end-of-line)
                 (and (nth 4 (syntax-ppss))
                      (save-excursion
                        (beginning-of-line)
                        (looking-at paragraph-start))
                      (looking-at "[[:space:]]*$")
                      (nth 4 (syntax-ppss next-bol))))
          (goto-char next-bol)))
      (when (save-excursion
              (and (nth 4 (syntax-ppss (line-beginning-position 1)))
                   (looking-at "[[:space:]]*\\*/")))
        (goto-char (line-end-position 0)))
      (funcall body))))

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
