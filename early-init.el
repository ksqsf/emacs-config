;;; -*- lexical-binding: t; -*-

;; Restore dumped load-path
(when (boundp '+saved-load-path-during-dump)
  (message "Starting from a dump file...")
  (setq load-path +saved-load-path-during-dump))

;; This is a dangerous value, and will be reset after Emacs is
;; initialized.
(setq gc-cons-threshold (* 1024 1024 1024))

;; Set package archives. Possibly set mirrors.
(defconst +i-am-in-china+ nil)
(with-eval-after-load 'package
  (if +i-am-in-china+
      (setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                           ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                           ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))))

(setq package-quickstart t)
(setq package-quickstart-file (expand-file-name "var/package-quickstart.el" user-emacs-directory))

;; Set frame parameters early to prevent flickering.
(setq default-frame-alist
      '((height . 50)
        (width . 120)
        (vertical-scroll-bars . nil)))

;; Must be set before loading use-package
(setq use-package-enable-imenu-support t)

;; Customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
