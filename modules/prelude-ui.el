;;; -*- lexical-binding: t; -*-

(defcustom prelude-ui-accent-color
  "brown2"
  "Accent color of UI elements. This affects the mode line."
  :group 'prelude
  :type '(color))

(defcustom prelude-ui-frame-alpha       ; TODO
  0.92
  "Default frame alpha."
  :group 'prelude)

;; Pixelwise resize
(setq frame-resize-pixelwise t)

;; Don't show useless UI elements
(add-hook 'after-init-hook
          #'(lambda ()
              (menu-bar-mode 1)
              (tool-bar-mode 0)))

;; My vendor settings!
;; 1. Choose a better background color (thanks to Xah Lee)
;; 2. Better default size
;;
;; I strongly advise you write equivalent settings in your ~/.Xresources!
(setq default-frame-alist nil)
(setq initial-frame-alist nil)
(if (display-graphic-p)
    (progn
      (setq default-frame-alist
            '((vertical-scroll-bars . nil))))
  (progn
    (setq default-frame-alist '((tool-bar-lines . 0)))))

;; Mac-specific settings
(when *is-a-mac*
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

;; Default theme
(use-package srcery-theme
  :config
  (load-theme 'srcery t)
  (set-face-attribute 'font-lock-comment-face nil :foreground "orange")
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "orange"))

;; Mode line
(use-package diminish
  :defer 1
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode))
  :init
  (setq doom-modeline-minor-modes nil)
  :config
  (set-face-attribute 'doom-modeline-bar nil :background prelude-ui-accent-color))

;; ;; I'm the winner ;-)
(use-package winner
  :defer t
  :bind (:map winner-mode-map
              ("C-c ," . winner-undo)
              ("C-c ." . winner-redo))
  :init
  (defvar winner-dont-bind-my-keys t)
  (add-hook 'after-init-hook #'winner-mode))

;; ;; Eye candy icons
(use-package all-the-icons
  :defer t)

;; ;; Rainbow
(use-package rainbow-mode
  :commands (rainbow-mode))

;; ;; Enable ligatures
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

(provide 'prelude-ui)
