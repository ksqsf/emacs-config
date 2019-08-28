;;; -*- lexical-binding: t; -*-

(use-package paredit
  :hook ((lisp-mode emacs-lisp-mode lisp-interaction-mode) . paredit-mode)
  :config
  (add-hook 'lisp-interaction-mode-hook #'(lambda () (define-key paredit-mode-map "\C-j" nil))))

;; Scheme prefers Guile
(eval-after-load 'scheme
  (setq scheme-program-name "guile"))

(eval-after-load 'guile
  (setq geiser-default-implementation 'guile))

(provide 'prelude-lang-lisp)
