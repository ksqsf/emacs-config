;;; -*- lexical-binding: t; -*-
;;; This file modifies some of the essential behaviors of Emacs, and
;;; likely everyone wants them, thus the name "core".

;; GC less conservative.  No more frequently than every 100 MiB.
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 100 1024 1024))))
(add-hook 'focus-out-hook #'garbage-collect)

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

;; Delete the selected region when press <del>
(setq delete-active-region t)
(delete-selection-mode t)

;; Disable fancy features when the file is too large
(global-so-long-mode t)

;; f2. 2C-mode can be invoked using C-x 6
(keymap-global-set "<f2>" #'follow-mode)

;; f11
(keymap-global-set "<f11>" #'toggle-frame-fullscreen)

;; Show column number
(column-number-mode t)

;; zap chars
(keymap-global-unset "M-z")
(keymap-global-set "M-z" #'zap-up-to-char)

;; C-z is too easy to hit, and you can use C-x z instead
(keymap-global-unset "C-z")

;; Auto revert
(global-auto-revert-mode)

;; why, but why, emacs!
(setq load-prefer-newer t)

;; Use `ibuffer' as a drop-in replacement of `list-buffers' (C-x C-b).
;; The former should provide much more functions.
(defalias 'list-buffers 'ibuffer)

;; Better undo
(use-package vundo
  :bind
  ("C-x u" . vundo))
(use-package undohist)
(keymap-global-set "C-?" #'undo-only)
(add-hook 'after-init-hook
          #'(lambda ()
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
  :bind (("M-o" . other-window)
         ("C-x o" . ace-window)
         ("C-x C-o" . ace-window))  ;; was delete-blank-lines
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))

(use-package windmove
  :ensure nil
  :bind (("C-<left>" . windmove-left)
         ("C-<right>" . windmove-right)
         ("C-<up>" . windmove-up)
         ("C-<down>" . windmove-down)))

;; Recentf
(use-package recentf
  :ensure nil
  :hook ((after-init . recentf-mode))
  :config
  (setq recentf-auto-cleanup 'never)
  (run-with-idle-timer 30 t #'(lambda () (k|with-suppressed-message (recentf-save-list))))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; recursive edit
(defun isearch-open-recursive-edit ()
  "Use `\\\\[exit-recursive-edit]' to end the recursive edit. Or
  use `abort-recursive-edit' to exit the recursive edit and
  cancel the previous search."
  (interactive)
  (with-isearch-suspended (recursive-edit)))

(keymap-global-set "s-x" #'exit-recursive-edit)
(define-key isearch-mode-map (kbd "s-r") #'isearch-open-recursive-edit)

;; Meaningful M-<, M->
(use-package beginend
  :disabled
  :defer 1
  :config
  (beginend-global-mode))

;; Mac is stupid
(when k|mac
  (setq default-directory "~/")
  (setq command-line-default-directory "~/")
  (setenv "LC_ALL" "zh_CN.utf-8")
  (setenv "LANG" "zh_CN.utf-8")

  (setq mac-command-modifier 'meta)

  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-arguments '("-l"))
    :config
    (exec-path-from-shell-copy-envs '("PATH")))

  (keymap-global-set "s-c" #'clipboard-kill-ring-save)
  (keymap-global-set "s-v" #'clipboard-yank)
  (keymap-global-set "s-w" #'delete-frame)
  (keymap-global-set "s-s" #'save-buffer)
  (keymap-global-set "s-t" #'split-window-horizontally)
  (keymap-global-set "s-T" #'split-window-vertically)
  (keymap-global-set "s-o" #'ace-window)
  (keymap-global-set "s-x" #'execute-extended-command)
  (keymap-global-set "s-n" #'make-frame-command)

  ;; Mac will interpret the Insert key on a PC keyboard as Help
  (define-key key-translation-map (kbd "<help>") (kbd "<insert>"))

  ;; macport feature: too slow!
  (setq mac-mouse-wheel-smooth-scroll nil))

;; Disable keys I don't use.
(keymap-global-unset "C-x C-n")

;; Jump
(keymap-global-set "M-]" #'forward-paragraph)
(keymap-global-set "M-[" #'backward-paragraph)

;; dwim
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-c" #'capitalize-dwim)

;; Ripgrep
(use-package ripgrep
  :commands (ripgrep-regexp ripgrep-search-mode))

;; Auto insert
(use-package autoinsert
  :ensure nil
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
(keymap-global-set "C-w" #'k/kill-region-or-backward-word)

;; auth sources
(setq auth-sources '("~/.authinfo" "~/.netrc"))

;; swap C-h p and C-h P
(keymap-global-set "C-h p" #'describe-package)
(keymap-global-set "C-h P" #'finder-by-keyword)

;; jump instantly
(use-package avy
  :bind ("M-j" . avy-goto-char-timer))

;; show-paren-mode
(setq show-paren-context-when-offscreen t)

;; align
(keymap-global-set "C-c C-a" #'align)

;; enable yasnippet everywhere
(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :commands (yas-global-mode yas-minor-mode)
  :hook (after-init . yas-global-mode)
  :config
  (yas-reload-all))

;; save buffers when focus out
(add-hook 'focus-out-hook #'(lambda () (save-some-buffers t)))

;; Enable on-the-fly editing enhancements
(electric-indent-mode +1)
(electric-pair-mode +1)

;; Follow US English conventions by default.  This is required for
;; anything to be included in official Emacs distribution.
(setq sentence-end-double-space t)

;; Enhance isearch.
(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t))

;; Sublime-like multiple cursors.
(use-package multiple-cursors
  :commands (mc/add-cursor-on-click)
  :bind ("A-<mouse-1>" . mc/add-cursor-on-click))

(provide 'prelude-core)
