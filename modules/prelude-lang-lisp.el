;;; -*- lexical-binding: t; -*-
;;; Lisp
(ensure-package 'paredit)

(defun prelude--enter-lisp ()
  (paredit-mode 1))

(add-hook 'lisp-mode-hook #'prelude--enter-lisp)
(add-hook 'emacs-lisp-mode-hook #'prelude--enter-lisp)

;; Scheme prefers Guile
(eval-after-load 'scheme
  (setq scheme-program-name "guile"))

(eval-after-load 'guile
  (setq geiser-default-implementation 'guile))

(provide 'prelude-lang-lisp)
