;;; This file modifies some of the essential behaviors of Emacs, and
;;; likely everyone wants them, thus the name "core".

;; GC less conservative.  No more frequently than every 10 MiB.
(setq gc-cons-threshold (* 10 1024 1024))

;; Stop Emacs littering init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; MELPA Stable
(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa-stable/")
			 ("melpa-unstable" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)

;; Don't blink!
(blink-cursor-mode 0)

;; Don't let Emacs hurt my ears
(setq visible-bell t)

;; Don't show messages that I don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; f2. 2C-mode can be invoked using C-x 6
(global-set-key (kbd "<f2>") #'follow-mode)

;; f11
(global-set-key (kbd "<f11>") #'toggle-frame-fullscreen)

;; Show column number
(column-number-mode t)

;; zap chars
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z t") #'zap-up-to-char)
(global-set-key (kbd "M-z f") #'zap-to-char)

;; C-z is too easy to hit, and you can use C-x z instead
(global-unset-key (kbd "C-z"))

;; Use `ibuffer' as a drop-in replacement of `list-buffers' (C-x C-b).
;; The former should provide much more functions.
(defalias 'list-buffers 'ibuffer)

;; For quick undo, use C-/
;; For tree-style undo history, use C-x u
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t
      undo-tree-visualizer-relative-timestamps t
      undo-tree-visualizer-diff t
      undo-tree-enable-undo-in-region t)

(provide 'prelude-core)
