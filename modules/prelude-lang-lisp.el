;;; -*- lexical-binding: t; -*-
;;; Lisp
(ensure-package 'paredit)

(defun prelude/enter-lisp ()
  (paredit-mode 1))

(defun prelude/enter-lisp-interaction ()
  (define-key paredit-mode-map "\C-j" nil))

(add-hook 'lisp-mode-hook #'prelude/enter-lisp)
(add-hook 'emacs-lisp-mode-hook #'prelude/enter-lisp)
(add-hook 'lisp-interaction-mode-hook #'prelude/enter-lisp-interaction)

;; Scheme prefers Guile
(eval-after-load 'scheme
  (setq scheme-program-name "guile"))

(eval-after-load 'guile
  (setq geiser-default-implementation 'guile))

(provide 'prelude-lang-lisp)
