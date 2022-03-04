;;; -*- lexical-binding: t; -*-

;;
;; Emacs Lisp
;;
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-buffer)


;;
;; Common Lisp
;;

(setq inferior-lisp-program "sbcl")

(use-package sly
  :commands (sly sly-mode)
  :mode (("\\.lisp\\'" . lisp-mode)))

(eval-after-load 'scheme
  (setq scheme-program-name "racket"))

(use-package geiser
  :mode (("\\.ss\\'" . geiser-mode))
  :config
  (setq geiser-default-implementation 'racket))


;;
;; My own SMTLib mode
;;
(use-package smtlib2-mode
  :ensure nil
  :mode (("\\.smt2?\\'" . smtlib2-mode)
         ("\\.sygus2?\\'" . smtlib2-mode)))

(provide 'prelude-lang-lisp)
