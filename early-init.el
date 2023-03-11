;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 1024 1024 1024))

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))

(setq buffer-file-coding-system 'utf-8-unix)

;; Set frame parameters early to prevent flickering.
(setq default-frame-alist '((height . 50)
                            (width . 120)
                            (vertical-scroll-bars . nil)))
(setq initial-frame-alist '((alpha . 0.97)))
