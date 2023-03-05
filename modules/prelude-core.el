;;; -*- lexical-binding: t; -*-
;;; This file modifies some of the essential behaviors of Emacs, and
;;; likely everyone wants them, thus the name "core".

(setq confirm-kill-emacs 'yes-or-no-p)

;; GC.
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 1000 1024 1024))))
(add-hook 'focus-out-hook #'garbage-collect)
(use-package gcmh
  :hook (after-init . gcmh-mode))

;; auto revert everything, including dired.
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(when auto-revert-use-notify
  (setq auto-revert-avoid-polling t))

;; Save clipboard before changing the kill state.  The typical
;; scenario is that, if you copy something from other programs, then
;; use Emacs to kill a word, the old clipboard data will be lost.
;; 1024 seems large enough.
(setq save-interprogram-paste-before-kill 1024)
(setq kill-do-not-save-duplicates t)

;; Move backups away
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Don't recenter to the middle of the screen
(setq recenter-positions '(top 0.3 bottom))

;; Don't let Emacs hurt my ears
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Don't show messages that I don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Delete the selected region when press <del>
(setq delete-active-region t)
(delete-selection-mode t)

;; Scroll: avoid surprises (try to make scrolling "continous")
(setq scroll-margin 1)
(setq scroll-preserve-screen-position t)

;; Disable fancy features when the file is too large
(global-so-long-mode t)

;; f2. 2C-mode can be invoked using C-x 6
(global-set-key (kbd "<f2>") #'follow-mode)

;; f11
(global-set-key (kbd "<f11>") #'toggle-frame-fullscreen)

;; Show column number
(column-number-mode t)

;; zap chars
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z") #'zap-up-to-char)

;; Remap repeat to C-z.
(global-set-key (kbd "C-z") #'repeat)

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
(global-set-key (kbd "C-?") #'undo-only)
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
  :custom
  (recentf-max-saved-items 500)
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

(global-set-key (kbd "s-x") #'exit-recursive-edit)
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
  (setq polling-period 0.05)
  (setenv "LC_ALL" "zh_CN.utf-8")
  (setenv "LANG" "zh_CN.utf-8")

  (use-package exec-path-from-shell
    :demand t
    :init
    (setq exec-path-from-shell-arguments '("-l"))
    :config
    (exec-path-from-shell-copy-envs '("PATH")))

  ;; prefer Hyper because Super is already polluted.
  (setq ns-right-command-modifier 'hyper)
  (setq ns-alternate-modifier 'hyper)
  (setq ns-command-modifier 'meta)

  (global-set-key (kbd "H-t") tab-prefix-map)
  (global-set-key (kbd "H-n") #'tab-new)
  (global-set-key (kbd "H-w") #'tab-close)
  (global-set-key (kbd "H-z") #'tab-undo)

  ;; (global-set-key (kbd "s-c") #'clipboard-kill-ring-save)
  ;; (global-set-key (kbd "s-v") #'clipboard-yank)
  ;; (global-set-key (kbd "s-w") #'delete-frame)
  ;; (global-set-key (kbd "s-s") #'save-buffer)
  ;; (global-set-key (kbd "s-t") #'split-window-horizontally)
  ;; (global-set-key (kbd "s-T") #'split-window-vertically)
  ;; (global-set-key (kbd "s-o") #'ace-window)
  ;; (global-set-key (kbd "s-x") #'execute-extended-command)
  ;; (global-set-key (kbd "s-n") #'make-frame-command)

  ;; Mac will interpret the Insert key on a PC keyboard as Help
  (define-key key-translation-map (kbd "<help>") (kbd "<insert>"))

  ;; macport feature: too slow!
  (setq mac-mouse-wheel-smooth-scroll nil)

  ;; mitigate tearing
  (setq recenter-redisplay t)

  ;; Prefer GNU ls to Darwin's ls
  (when (executable-find "gls")
    (setq insert-directory-program "gls")))

;; Disable keys I don't use.
(global-unset-key (kbd "C-x C-n"))

;; Jump
(global-set-key (kbd "M-]") #'forward-paragraph)
(global-set-key (kbd "M-[") #'backward-paragraph)

;; dwim
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-c") #'capitalize-dwim)

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
  (defun treesit-mark-bigger-node ()
    (let* ((root (treesit-buffer-root-node))
           (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
        (when-let ((node (treesit-node-parent node)))
          (setq node-start (treesit-node-start node)
                node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))
  (define-advice set-mark-command (:before-while (arg))
    "Repeat C-SPC to expand region."
    (interactive "P")
    (if (eq last-command 'set-mark-command)
        (progn
          (er/expand-region 1)
          nil)
      t))
  :config
  (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node))

;; C-w to kill word when region is inactive
(defun k/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") #'k/kill-region-or-backward-word)

;; auth sources
(setq auth-sources '("~/.authinfo" "~/.netrc"))

;; swap C-h p and C-h P
(global-set-key (kbd "C-h p") #'describe-package)
(global-set-key (kbd "C-h P") #'finder-by-keyword)

;; jump instantly
(use-package avy
  :bind ("M-j" . avy-goto-char-timer))

;; show-paren-mode
(setq show-paren-context-when-offscreen t)

;; align
(global-set-key (kbd "C-c C-a") #'align)

;; diminish
(use-package diminish)

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

;; Graphical Emacs should serve as a daemon.
(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))

;; Give the scratch buffer a shortcut.
(global-set-key (kbd "C-x C-n") #'scratch-buffer)

;; Reverse yank-pop.  By default, it is C-u M-y, but it's not as
;; intuitive.  The "Shift reverse" metaphor seems well established.
(global-set-key (kbd "M-Y") #'(lambda () (interactive) (yank-pop -1)))

;; find file at point
(use-package ffap
  :ensure nil
  :config
  (defun ffap-line ()
    "Like `ffap' but also guesses the target line.

Useful for reading Python exception traces."
    (interactive)
    (let ((current-line (buffer-substring (line-beginning-position) (line-end-position)))
          line-no)
      (save-match-data
        (if (string-match "line \\([0-9]+\\)" current-line 0)
            (setq line-no (string-to-number (match-string 1 current-line)))
          (string-match "\\([0-9]+\\)" current-line 0)
          (setq line-no (string-to-number (match-string 1 current-line)))))
      (ffap)
      (when line-no
        (goto-line line-no)))))

(use-package dabbrev
  :ensure nil
  :config
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(defun sudoedit ()
  "Edit the current file using Sudo."
  (interactive)
  (find-file (concat "/sudo::" buffer-file-name)))

(use-package adaptive-wrap
  :init
  (setq adaptive-wrap-extra-indent 2)
  (defun turn-on-adaptive-wrap ()
    (setq-local word-wrap t)
    (adaptive-wrap-prefix-mode t)))

;; Save positions in files
(toggle-save-place-globally)

(provide 'prelude-core)
