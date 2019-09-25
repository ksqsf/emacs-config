;;; -*- lexical-binding: t; -*-

(use-package paredit
  :hook ((lisp-mode emacs-lisp-mode lisp-interaction-mode) . paredit-mode)
  :config
  (add-hook 'lisp-interaction-mode-hook #'(lambda () (define-key paredit-mode-map "\C-j" nil))))

(eval-after-load 'scheme
  (setq scheme-program-name "mechanics"))

(eval-after-load 'guile
  (setq geiser-default-implementation 'guile))

(defun mechanics ()
  (interactive)
  (run-scheme "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib"))

(provide 'prelude-lang-lisp)
