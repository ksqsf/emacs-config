;;; -*- lexical-binding: t; -*-

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-buffer)

(setq inferior-lisp-program "ccl64")

(use-package paredit
  :diminish "Ⓟ"
  :hook ((lisp-mode emacs-lisp-mode lisp-interaction-mode) . paredit-mode)
  :bind (("M-<right>" . paredit-forward-slurp-sexp)
         ("M-<left>" . paredit-backward-slurp-sexp)
         ("M-<up>" . paredit-splice-sexp-killing-backward)
         ("M-k" . paredit-kill)
         ("M-a" . paredit-backward)
         ("M-e" . paredit-forward))
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
  :disabled
  :commands (sly sly-mode)
  :mode (("\\.lisp\\'" . lisp-mode)))

;; replace old z3-mode
(use-package smtlib2-mode
  :ensure nil
  :mode (("\\.smt2?\\'" . smtlib2-mode)
         ("\\.sygus2?\\'" . smtlib2-mode)))

(provide 'prelude-lang-lisp)
