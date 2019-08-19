;;; -*- lexical-binding: t; -*-
;;; This file modifies some of the essential behaviors of Emacs, and
;;; likely everyone wants them, thus the name "core".

;; GC less conservative.  No more frequently than every 20 MiB.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 20 1024 1024))))
(add-hook 'focus-out-hook #'garbage-collect)

;; Stop Emacs littering init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook #'(lambda () (load custom-file)))

;; MELPA Stable
(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))
(package-initialize)

;; Don't blink!
(blink-cursor-mode 0)

;; Don't let Emacs hurt my ears
(setq visible-bell t)

;; Don't show messages that I don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Delete selection when try to insert text
(delete-selection-mode t)

;; f2. 2C-mode can be invoked using C-x 6
(global-set-key (kbd "<f2>") #'follow-mode)

;; f11
(global-set-key (kbd "<f11>") #'toggle-frame-fullscreen)

;; Show column number
(column-number-mode t)

;; zap chars
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z") #'zap-up-to-char)

;; C-z is too easy to hit, and you can use C-x z instead
(global-unset-key (kbd "C-z"))

;; Auto revert
(global-auto-revert-mode)

;; why, but why, emacs!
(setq load-prefer-newer t)

;; Use `ibuffer' as a drop-in replacement of `list-buffers' (C-x C-b).
;; The former should provide much more functions.
(defalias 'list-buffers 'ibuffer)

;; For quick undo, use C-/
;; For tree-style undo history, use C-x u
(ensure-package 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t
      undo-tree-visualizer-relative-timestamps t
      undo-tree-visualizer-diff t
      undo-tree-enable-undo-in-region t)

;; Use ace-window for quick window navigation
;; Sorry, `other-window', but you are too weak!
(ensure-package 'ace-window)
(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0)))))
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-scope 'frame)
(global-set-key (kbd "M-o") #'ace-window)
(global-set-key (kbd "C-x o") #'ace-window)

;; Recentf
(require 'recentf)
(setq recentf-exclude '("recentf"))
(setq recentf-auto-cleanup 'never)
(run-with-idle-timer 30 t #'(lambda () (with-suppressed-message (recentf-save-list))))
(recentf-mode t)

;; recursive edit
(defun isearch-open-recursive-edit ()
  "Use `\\\\[exit-recursive-edit]' to end the recursive edit. Or
  use `abort-recursive-edit' to exit the recursive edit and
  cancel the previous search."
  (interactive)
  (with-isearch-suspended (recursive-edit)))

(global-set-key (kbd "s-x") #'exit-recursive-edit)
(define-key isearch-mode-map (kbd "s-r") #'isearch-open-recursive-edit)

;; Meaningful M-<, M->
(ensure-package 'beginend)
(beginend-global-mode 1)

;; Mac is stupid
(when *is-a-mac*
  (ensure-package 'exec-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-copy-envs '("PATH" "MANPATH")))

(provide 'prelude-core)
