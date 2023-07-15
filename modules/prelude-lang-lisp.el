;;; -*- lexical-binding: t; -*-

(use-package paredit
  :hook (emacs-lisp . paredit-mode))

;;
;; Emacs Lisp
;;
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "M-<up>") #'raise-sexp)
(add-to-list 'auto-mode-alist '("\\.el\\.disabled\\'" . emacs-lisp-mode))


;;
;; Common Lisp
;;

(setq inferior-lisp-program "sbcl")

(use-package sly
  :commands (sly sly-mode)
  :mode (("\\.lisp\\'" . lisp-mode))
  :config
  (remove-hook 'lisp-mode-hook 'sly-editing-mode))

;;
;; Scheme
;;
(eval-after-load 'scheme
  (setq scheme-program-name "guile"))

(use-package geiser
  :mode (("\\.ss\\'" . scheme-mode))
  :hook ((scheme-mode . turn-on-geiser-mode)
         (geiser-repl-mode . electric-pair-local-mode))
  :custom
  (geiser-default-implementation nil)
  :config
  (use-package geiser-guile)
  (use-package geiser-racket))

;;
;; Racket
;;
(use-package racket-mode
  :mode (("\\.rkt\\'" . racket-mode))
  :hook (racket-mode . racket-xp-mode))


;;
;; My own SMTLib mode
;;
(use-package smtlib2-mode
  :ensure nil
  :mode (("\\.smt2?\\'" . smtlib2-mode)
         ("\\.sygus2?\\'" . smtlib2-mode)))

;; Clojure
(use-package cider
  :defer t)

(provide 'prelude-lang-lisp)
