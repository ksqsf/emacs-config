;;; -*- lexical-binding: t; -*-
;;; This file modifies some of the essential behaviors of Emacs, and
;;; likely everyone wants them, thus the name "core".

;; GC less conservative.  No more frequently than every 50 MiB.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 50 1024 1024))))
(add-hook 'focus-out-hook #'garbage-collect)

;; Stop Emacs littering init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook #'(lambda () (load custom-file)))

;; No-littering
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Move backups away
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Don't recenter to the middle of the screen
(setq recenter-positions '(top 0.25 bottom))

;; Don't let Emacs hurt my ears
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

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

;; Better undo
(use-package undohist)
(use-package undo-propose)
(global-set-key (kbd "C-?") #'undo-only)
(global-set-key (kbd "C-x u") #'undo-propose)
(add-hook 'after-init-hook
          '(lambda ()
             (require 'undohist)
             (undohist-initialize)

             ;; Patch to make undohist silent
             (define-advice undohist-recover-1 (:override ())
               (let* ((buffer (current-buffer))
                      (file (buffer-file-name buffer))
                      (undo-file (make-undohist-file-name file))
                      undo-list)
                 (when (and (undohist-recover-file-p file)
                            (file-exists-p undo-file)
                            (null buffer-undo-list))
                   (with-temp-buffer
                     (insert-file-contents undo-file)
                     (goto-char (point-min))
                     (let ((alist (undohist-decode (read (current-buffer)))))
                       (if (string= (md5 buffer) (assoc-default 'digest alist))
                           (setq undo-list (assoc-default 'undo-list alist))
                         (message "File digest doesn't match, so undo history will be discarded."))))
                   (when (consp undo-list)
                     (setq buffer-undo-list undo-list)))))))
(setq undohist-directory (no-littering-expand-var-file-name "undohist"))
(setq undohist-ignored-files '("\\.git/COMMIT_EDITMSG$"))

;; Use ace-window for quick window navigation
;; Sorry, `other-window', but you are too weak!
(use-package ace-window
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-swap-window))
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))

(use-package windmove
  :bind (("C-<left>" . windmove-left)
         ("C-<right>" . windmove-right)
         ("C-<up>" . windmove-up)
         ("C-<down>" . windmove-down)))

;; Recentf
(use-package recentf
  :hook ((after-init . recentf-mode))
  :config
  (setq recentf-auto-cleanup 'never)
  (run-with-idle-timer 30 t #'(lambda () (with-suppressed-message (recentf-save-list))))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

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
(use-package beginend
  :disabled
  :defer 1
  :config
  (beginend-global-mode))

;; Mac is stupid
(when *is-a-mac*
  (setq default-directory "~/")
  (setq command-line-default-directory "~/")
  (setenv "LC_ALL" "zh_CN.utf-8")

  (setq mac-command-modifier 'meta)

  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-arguments '("-l"))
    :config
    (exec-path-from-shell-copy-envs '("PATH")))

  (global-set-key (kbd "s-c") #'clipboard-kill-ring-save)
  (global-set-key (kbd "s-v") #'clipboard-yank)
  (global-set-key (kbd "s-w") #'delete-frame)
  (global-set-key (kbd "s-s") #'save-buffer)
  (global-set-key (kbd "s-t") #'split-window-horizontally)
  (global-set-key (kbd "s-T") #'split-window-vertically)
  (global-set-key (kbd "s-o") #'ace-window)
  (global-set-key (kbd "s-x") #'execute-extended-command)
  (global-set-key (kbd "s-n") #'make-frame-command)

  ;; Mac will interpret the Insert key on a PC keyboard as Help
  (define-key key-translation-map (kbd "<help>") (kbd "<insert>"))

  ;; macport feature: too slow!
  (setq mac-mouse-wheel-smooth-scroll nil))

;; Disable keys I don't use.
(global-unset-key (kbd "C-x C-n"))

;; Jump pages
(global-set-key (kbd "M-]") #'forward-page)
(global-set-key (kbd "M-[") #'backward-page)

;; dwim
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-c") #'capitalize-dwim)

;; Ripgrep
(use-package ripgrep
  :commands (ripgrep-regexp ripgrep-search-mode))

;; Auto insert
(use-package autoinsert
  :defer 1)

;; Expand region
(use-package expand-region
  :commands (er/expand-region)
  :bind (("C-=" . er/expand-region))
  :init
  (define-advice set-mark-command (:before-while (arg))
    "Repeat C-SPC to expand region."
    (interactive "P")
    (if (eq last-command 'set-mark-command)
        (progn
          (er/expand-region 1)
          nil)
      t)))

;; C-w to kill word when region is inactive
(defun k/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") #'k/kill-region-or-backward-word)

;; <del>Replace dabbrev-expand by hippie-expand</del>
;; hippie-expand does too much
;; (global-set-key (kbd "M-/") #'dabbrev-expand)

;; auth sources
(setq auth-sources '("~/.netrc"))

;; swap C-h p and C-h P
(global-set-key (kbd "C-h p") #'describe-package)
(global-set-key (kbd "C-h P") #'finder-by-keyword)

;; jump instantly
(use-package avy
  :bind ("M-j" . avy-goto-char-timer))

;;
(setq show-paren-context-when-offscreen t)

(provide 'prelude-core)
