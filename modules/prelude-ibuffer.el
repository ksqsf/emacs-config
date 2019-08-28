;;; -*- lexical-binding: t; -*-
;; ibuffer is chosen in favor of list-buffers in core.el

(use-package ibuffer
  :commands (ibuffer)
  :config
  (setq ibuffer-saved-filter-groups
        '(("Normal"
           ("Dired" (mode . dired-mode))
           ("Emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*compilation\\*")
                     (name . "^\\*dashboard\\*$")
                     (name . "^\\*Backtrace\\*$")
                     (name . "^\\*Packages\\*$")))
           ("Text" (or
                    (mode . org-mode)
                    (mode . markdown-mode)
                    (mode . text-mode)))
           ("Tex" (mode . tex-mode))
           ("Code" (or
                    (mode . emacs-lisp-mode)
                    (mode . haskell-mode)
                    (mode . rust-mode)
                    (mode . html-mode)
                    (mode . css-mode)
                    (mode . python-mode)
                    (mode . prog-mode)))
           ("Mait" (name . "^magit"))
           ("Help" (or
                    (name . "^\\*Help\\*$")
                    (name . "^\\*info\\*$")
                    (name . "^\\*Apropos\\*$")))
           ("Custom" (or
                      (mode . customi-mode)
                      (name . "^\\*Customize"))))))
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-default-sorting-mode 'filename/process)
  (add-hook 'ibuffer-mode-hook #'(lambda () (ibuffer-switch-to-saved-filter-groups "Normal"))))

(provide 'prelude-ibuffer)

