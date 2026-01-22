;;; -*- lexical-binding: t; -*-
;;; This file modifies some of the essential behaviors of Emacs, and
;;; likely everyone wants them, thus the name "core".

(setq confirm-kill-emacs 'yes-or-no-p)

;; GC.
(add-hook 'focus-out-hook #'garbage-collect)

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
(setq kill-region-dwim t)

;; Move backups away
(setq backup-directory-alist `(("/ssh:.*" . nil)
                               ("." . ,(no-littering-expand-var-file-name "backups"))))

;; Don't backup by rename
;; By default, Emacs creates backup B for file A by renaming A into B, then write new contents to a new file A'.
;; The original intention is probably handling hard links better. Plausible.
;; Alas, this method will destroy permission bits and ownership information!
;; Let's settle on the lesser evil.
;; (Another benefit is Tramp saving become faster combined with purely local backups.)
(setq backup-by-copying t)

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
(setq scroll-step 1)
(setq scroll-conservatively 10000)

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

;; Some useful editing commands deserve quicker access.
(global-set-key (kbd "C-x C-d") #'duplicate-dwim)
(setq duplicate-line-final-position 1)

;; why, but why, emacs!
(setq load-prefer-newer t)

;; Use `ibuffer' as a drop-in replacement of `list-buffers' (C-x C-b).
;; The former should provide much more functions.
(global-set-key [remap list-buffers] #'ibuffer)

;; Better undo
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-_") 'undo)  ; Generated in a terminal on C-/
(global-set-key (kbd "C-?") 'undo-redo)
(setq undo-limit (* 100 1024 1024))
(setq undo-strong-limit (* 200 1024 1024))
(setq undo-outer-limit (* 50 1024 1024))
(use-package vundo
  :bind ("C-x u" . vundo))
(use-package undohist
  :config
  (setq undohist-directory (no-littering-expand-var-file-name "undohist"))
  (push "\\.git/COMMIT_EDITMSG\\'" undohist-ignored-files)
  (push "dict.yaml\\'" undohist-ignored-files)
  (push "essay.txt\\'" undohist-ignored-files)
  (push tramp-file-name-regexp undohist-ignored-files)
  )
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

;; Window and frame navigation
(global-set-key (kbd "M-o") #'other-window)

;; Use ace-window for quick window navigation
;; Sorry, `other-window', but you are too weak!
(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("C-x C-o" . ace-window))  ;; was delete-blank-lines
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))

(use-package windmove
  :disabled ;; conflict with org!
  :ensure nil
  :init
  (windmove-default-keybindings) ;; S-<arrow>
  (windmove-delete-default-keybindings) ;; C-x S-<arrow>
  (windmove-swap-states-default-keybindings) ;; s-S-<arrow>
  )

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
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  ;; reduce 1rtt on remote files
  (defun +recentf-keep-p (file)
    (cond
     ((file-remote-p file))
     ((file-readable-p file))))
  (setq recentf-keep '(+recentf-keep-p)))

;; recursive edit
(defun isearch-open-recursive-edit ()
  "Use `\\\\[exit-recursive-edit]' to end the recursive edit. Or
  use `abort-recursive-edit' to exit the recursive edit and
  cancel the previous search."
  (interactive)
  (with-isearch-suspended (recursive-edit)))

;; Meaningful M-<, M->
(use-package beginend
  :disabled
  :defer 1
  :config
  (beginend-global-mode))

;; Mac is stupid
(when k|mac
  (when (string-prefix-p "/Applications" default-directory)
    (setq default-directory "~/")
    (setq command-line-default-directory "~/"))

  (use-package exec-path-from-shell
    :demand t
    :init
    (setq exec-path-from-shell-arguments '("-l"))
    :config
    (exec-path-from-shell-copy-envs '("PATH")))

  ;; prefer Hyper because Super is already polluted.
  (setq ns-right-command-modifier 'hyper)
  (setq ns-alternate-modifier 'super)
  (setq ns-command-modifier 'meta)

  ;; (global-set-key (kbd "H-t") tab-prefix-map)
  ;; (global-set-key (kbd "H-n") #'tab-new)
  ;; (global-set-key (kbd "H-w") #'tab-close)
  ;; (global-set-key (kbd "H-z") #'tab-undo)
  ;; (global-set-key (kbd "H-u") #'revert-buffer)

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

;; Never bind M-[ !
;; ESC [ is an important escape sequence prefix used by terminals.

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
        (when-let* ((node (treesit-node-parent node)))
          (setq node-start (treesit-node-start node)
                node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))
  (define-advice set-mark-command (:before-while (_arg))
    "Repeat C-SPC to expand region."
    (interactive "P")
    (if (eq last-command 'set-mark-command)
        (progn
          (er/expand-region 1)
          nil)
      t))
  :config
  (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node))

;; Mark
(setq global-mark-ring-max 50)
(setq mark-ring-max 50)
(setq set-mark-command-repeat-pop t)

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
  (isearch-lazy-count t)
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t))

;; Sublime-like multiple cursors.
(use-package multiple-cursors
  :bind ("M-<mouse-1>" . mc/add-cursor-on-click)
  :bind ("C->" . 'mc/mark-next-like-this)
  :bind ("C-<" . 'mc/mark-previous-like-this))
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
  :commands (ffap-line)
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
        (goto-char (point-min))
        (forward-line (1- line-no))))))

(use-package dabbrev
  :ensure nil
  :if (version< "29.0" emacs-version)
  :config
  (setopt dabbrev-ignored-buffer-modes
          (append dabbrev-ignored-buffer-modes
                  '(pdf-view-mode
                    magit-diff-mode
                    gdb-inferior-io-mode))))

(defun sudoedit ()
  "Edit the current file using Sudo."
  (interactive)
  (setq buf (current-buffer))
  (if (derived-mode-p 'dired-mode)
      (find-file (concat "/sudo::" default-directory))
    (find-file (concat "/sudo::" buffer-file-name)))
  (kill-buffer buf))

(use-package adaptive-wrap
  :init
  (setq adaptive-wrap-extra-indent 2)
  (defun turn-on-adaptive-wrap ()
    (setq-local word-wrap t)
    (adaptive-wrap-prefix-mode t)))

;; Save positions in files
(save-place-mode +1)

;; Utilities
(defun update ()
  "Update the auto-generated state of this Emacs config."
  (interactive)
  (byte-recompile-directory prelude-modules-dir)
  (package-quickstart-refresh))

(defun kill-all-buffers ()
  "Literally kill all buffers"
  ;; FIXME: this kills too many buffers, tab-line won't work anymore
  ;; after this.
  (interactive)
  (dolist (buf (buffer-list))
    (kill-buffer buf)))

(defun kill-other-buffers ()
  "Kill all buffers except for the current buffer."
  ;; FIXME: this kills too many buffers, tab-line won't work anymore
  ;; after this.
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(use-package hydra
  :demand t)

;; TRAMP performance
;; See https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq remote-file-name-inhibit-locks t)
(setq remote-file-name-inhibit-auto-save t)
(setq remote-file-name-inhibit-auto-save-visited t)
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)
(setq remote-file-name-inhibit-cache 600)
(setq remote-file-name-access-timeout 1)
(setq tramp-use-scp-direct-remote-copying t)
(setq tramp-copy-size-limit (* 1024 1024))
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))
(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)
(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; TRAMP shells
(setq tramp-default-remote-shell "/bin/bash")
(setq vterm-tramp-shells '(("docker" "/bin/sh")
                           ("ssh" "/bin/bash")))


;; Bind C-h C-p to profiler (was: view-emacs-problems)
(defun profiler-dwim ()
  (interactive)
  (require 'profiler)
  (if (or (profiler-cpu-running-p) (profiler-memory-running-p))
      (progn
        (profiler-report)
        (profiler-stop))
    (progn
      (profiler-reset)
      (profiler-start 'cpu))))
(global-set-key (kbd "C-h C-p") #'profiler-dwim)

;; CAVEAT: to be compatible to terminals, do NOT bind:
;; M-[, M-], M-O, etc.

;; Directly open network resources
(url-handler-mode +1)

;; Indicate the depth of recursive editing.
(minibuffer-depth-indicate-mode +1)

;; Use isearch in minibuffre
;; This enables rime in isearch and also other navigational keys.
(use-package isearch-mb
  :hook (after-init . isearch-mb-mode))

;; Make keyboard-quit smarter
(defun +keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))
(global-set-key [remap keyboard-quit] #'+keyboard-quit-dwim)

(provide 'prelude-core)
