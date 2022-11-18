;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")))

(setq buffer-file-coding-system 'utf-8-unix)
