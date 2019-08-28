;;; -*- lexical-binding: t; -*-
;;; Config for C/C++/Java/...

(use-package cc-mode
  :defer t
  :requires (rainbow-mode)
  :init
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "k&r")
                          (c++-mode . "stroustrup")
                          (other . "gnu")))
  :config
  (add-hook 'c-mode-common-hook #'subword-mode)
  (add-hook 'c-mode-common-hook #'(lambda () (rainbow-mode nil)))
  (define-key c-mode-map (kbd "<f5>") #'compile)
  (define-key c++-mode-map (kbd "<f5>") #'compile)
  (define-key c-mode-base-map (kbd "[") (double-tap-to-insert ?\())
  (define-key c-mode-base-map (kbd "]") (double-tap-to-insert ?\)))
  (define-key c-mode-base-map (kbd "'") (double-tap-to-insert ?\"))
  (define-key c-mode-base-map (kbd "`") (double-tap-to-insert ?\")))

(provide 'prelude-lang-cc)
