;;; -*- lexical-binding: t; -*-
;; ibuffer is chosen in favor of list-buffers in core.el

(use-package ibuffer
  :ensure nil
  :commands (ibuffer ibuffer-switch-to-saved-filter-groups)
  :hook (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "Normal")))
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
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
                    (mode . rustic-mode)
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
                      (name . "^\\*Customize")))
           ("IRC" (or
                   (mode . erc-mode)))
           ("Telega" (or
                      (mode . telega-root-mode)
                      (mode . telega-chat-mode))))))
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-default-sorting-mode 'filename/process)

  (use-package all-the-icons-ibuffer
    :commands (all-the-icons-ibuffer-mode)))

(provide 'prelude-ibuffer)
