;;; -*- lexical-binding: t; -*-
;; Don't show useless UI elements
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; My vendor settings!
;; 1. Choose a better background color (thanks to Xah Lee)
;; 2. Better default size
;;
;; I strongly advise you write equivalent settings in your ~/.Xresources!
(setq default-frame-alist nil)
(setq initial-frame-alist nil)
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '((alpha . 0.92))))
  (progn
    (setq initial-frame-alist '((tool-bar-lines . 0)))
    (setq default-frame-alist '((tool-bar-lines . 0)))))

;; Mac-specific settings
(when *is-a-mac*
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist)
  (push '(ns-transparent-titlebar . t) initial-frame-alist)
  (push '(ns-appearance . dark) initial-frame-alist))

;; Use srcery theme by default
(use-package srcery-theme
  :init
  (load-theme 'srcery t))

;; Mode line
(use-package diminish
  :disabled
  :defer 1
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

(use-package doom-modeline
  :init
  (setq doom-modeline-minor-modes nil)
  (doom-modeline-mode t))

;; I'm the winner ;-)
(use-package winner
  :defer t
  :bind (:map winner-mode-map
              ("C-c ," . winner-undo)
              ("C-c ." . winner-redo))
  :init
  (defvar winner-dont-bind-my-keys t)
  (add-hook 'after-init-hook #'winner-mode))

;; Eye candy icons
(use-package all-the-icons
  :defer t)

;; Make the cursor stand out.
(global-hl-line-mode t)

;; Rainbow
(use-package rainbow-mode
  :commands (rainbow-mode)
  :hook (prog-mode))

(provide 'prelude-ui)
