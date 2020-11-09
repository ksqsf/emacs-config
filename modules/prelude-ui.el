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
(add-to-list 'initial-frame-alist '(height . 55))
(add-to-list 'initial-frame-alist '(width . 120))

;; Mac-specific settings
(when *is-a-mac*
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

;; Default theme
(use-package srcery-theme
  :disabled
  :config
  (load-theme 'srcery t)
  (set-face-attribute 'font-lock-comment-face nil :foreground "orange")
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "orange"))

;; Mode line
(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package doom-modeline
  :disabled
  :hook ((after-init . doom-modeline-mode))
  :init
  (setq doom-modeline-minor-modes nil)
  :config
  (set-face-attribute 'doom-modeline-bar nil :background prelude-ui-accent-color))

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

;; Rainbow
(use-package rainbow-mode
  :commands (rainbow-mode))

;; Enable ligatures
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

;; Auto set margins
(use-package perfect-margin
  :disabled
  :quelpa (perfect-margin :fetcher github :repo "mpwang/perfect-margin")
  :custom
  (perfect-margin-visible-width 128)
  :config
  (setq perfect-margin-ignore-regexps '("^minibuf"))
  (perfect-margin-mode t)
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll))))

;; Dashboard
(use-package dashboard
  :disabled
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((projects . 5)
                          (bookmarks . 5)
                          (recents . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

;; Cycle themes
(defcustom prelude-themes
  (list 'doom-monokai-pro) 
  "Themes that you may want to use."
  :group 'prelude)

(defun cycle-theme ()
  "Cycle around `prelude-themes'."
  (interactive)
  (require 'dash)
  (if (> (length prelude-themes) 1)
      (let ((next (car prelude-themes)))
        (dolist (current custom-enabled-themes)
          (disable-theme current))
        (if (listp next)
            (dolist (theme next)
              (load-theme theme t))
          (load-theme next t))
        (setq prelude-themes (append (cdr prelude-themes)
                                     (list (car prelude-themes)))))))

(provide 'prelude-ui)
