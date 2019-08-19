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

;; Use dracula theme by default
(ensure-package 'srcery-theme)
(load-theme 'srcery t)

;; Mode line
(ensure-package 'diminish)
(ensure-package 'doom-modeline)

(doom-modeline-mode t)
(setq doom-modeline-minor-modes nil)

(diminish 'undo-tree-mode)
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)

;; I'm the winner ;-)
(defvar winner-dont-bind-my-keys t)
(winner-mode t)
(define-key winner-mode-map (kbd "C-c ,") 'winner-undo)
(define-key winner-mode-map (kbd "C-c .") 'winner-redo)

;; Eye candy icons
(ensure-package 'all-the-icons)

;; Make the cursor stand out.
(global-hl-line-mode t)

(provide 'prelude-ui)
