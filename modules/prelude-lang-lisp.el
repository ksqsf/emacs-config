;;; -*- lexical-binding: t; -*-

(setq inferior-lisp-program "ccl64")

(use-package paredit
  :hook ((lisp-mode emacs-lisp-mode lisp-interaction-mode) . paredit-mode)
  :config
  (add-hook 'lisp-interaction-mode-hook #'(lambda () (define-key paredit-mode-map "\C-j" nil))))

(eval-after-load 'scheme
  (setq scheme-program-name "mechanics"))

(use-package geiser
  :mode (("\\.ss\\'" . geiser-mode))
  :config
  (setq geiser-default-implementation 'chez))

(defun mechanics ()
  (interactive)
  (run-scheme "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib"))

(use-package sly
  :mode (("\\.lisp\\'" . sly-mode))
  :commands (sly-mode))

(provide 'prelude-lang-lisp)
