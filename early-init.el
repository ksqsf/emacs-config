;;; -*- lexical-binding: t; -*-

;; Restore dumped load-path
(when (boundp '+saved-load-path-during-dump)
  (message "Starting from a dump file...")
  (setq load-path +saved-load-path-during-dump))

;; This is a dangerous value, and will be reset after Emacs is
;; initialized.
(setq gc-cons-threshold (* 1024 1024 1024))

;; Prefer ELPA mirrors in China.
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
(setq package-quickstart t)
(setq package-quickstart-file (expand-file-name "var/package-quickstart.el" user-emacs-directory))

;; This is optional.
(setq buffer-file-coding-system 'utf-8-unix)

;; Set frame parameters early to prevent flickering.
(setq default-frame-alist
      '((height . 50)
        (width . 120)
        (vertical-scroll-bars . nil)))

;; Must be set before loading use-package
(setq use-package-enable-imenu-support t)

;; Customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
