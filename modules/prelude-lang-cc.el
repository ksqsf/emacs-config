;;; -*- lexical-binding: t; -*-
;;; Config for C/C++/Java/...

(defvar c-newline-and-indent-regexp "\\s)")

(use-package cc-mode
  :defer t
  :config
  (defun c-newline-and-indent ()
    "Open one more line when the next char is a closing paren.

The exact behavior can be controlled by
`c-newline-and-indent-regexp'.

This command relies on `c-indent-line' so it only works in CC
Mode."
    (interactive)
    (newline)
    (save-excursion
      (when (looking-at-p c-newline-and-indent-regexp)
        (newline)
        (c-indent-line)))
    (c-indent-line))

  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "k&r")
                          (c++-mode . "stroustrup")
                          (other . "gnu")))
  (add-hook 'c-mode-common-hook #'subword-mode)
  (add-hook 'c-mode-common-hook #'smartparens-mode)
  (add-hook 'c-mode-common-hook #'(lambda () (rainbow-mode 0)))
  (define-key c-mode-map (kbd "<f5>") #'compile)
  (define-key c++-mode-map (kbd "<f5>") #'compile)
  (define-key c-mode-base-map (kbd "[") (double-tap-to-insert ?\())
  (define-key c-mode-base-map (kbd "]") (double-tap-to-insert ?\)))
  (define-key c-mode-base-map (kbd "'") (double-tap-to-insert ?\"))
  (define-key c-mode-base-map (kbd "`") (double-tap-to-insert ?\"))
  (define-key c-mode-base-map (kbd "RET") #'c-newline-and-indent))

(provide 'prelude-lang-cc)
