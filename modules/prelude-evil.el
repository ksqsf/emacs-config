;;; -*- lexical-binding: t; -*-

;; Currently, I consider this an ``addition'' -- my config still works if you disable this.
;; Therefore, evil is loaded AFTER other modules, and other modules should not assume the presence of evil.

(use-package evil
  :demand t
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-abbrev-expand-on-insert-exit t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-disable-insert-state-bindings t)
  :bind (:map evil-normal-state-map
              (("C-e" . end-of-line)
               ("C-r" . isearch-backward)
               ("U" . evil-redo)
               ("M-." . xref-find-definitions)))
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init)
  (diminish 'evil-collection-unimpaired-mode))

(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after (evil org)
  :demand t
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; Quit corfu on Escape
(with-eval-after-load 'corfu
  (evil-define-key 'insert corfu-map (kbd "<escape>") 'corfu-quit))

;;; Leader key
(use-package evil-leader
  :demand t
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>"))

;;; Keybindings
(evil-leader/set-key
    "SPC" 'projectile-switch-to-buffer
    ;; file
    "fb" 'bookmark-bmenu-list
    ;; buffer
    "bi" 'ibuffer
    "bb" 'switch-to-buffer
    "bc" 'clone-indirect-buffer
    "bd" 'delete-trailing-lines
    "bu" 'revert-buffer
    ;; projectile
    "pp" 'projectile-switch-project
    "ps" 'projectile-vterm
    "pb" 'projectile-ibuffer
    "pk" 'projectile-kill-buffers
    "pg" 'projectile-grep
    "pa" 'projectile-find-other-file-other-window
    ;; profiler
    "PP" 'profiler-start
    "PS" 'profiler-stop
    ;; gptel
    "gg" 'gptel
    "gm" 'gptel-menu
    "gp" 'gptel-system-prompt
    "gs" 'gptel-send)

(with-eval-after-load 'haskell-mode
  (evil-define-key 'normal haskell-mode-map (kbd "gz") 'haskell-interactive-switch))

(with-eval-after-load 'gptel
  (evil-define-key 'motion 'global (kbd "gs") 'gptel-send)
  (evil-define-key 'visual 'global (kbd "gs") 'gptel-send))

(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map [tab] 'org-cycle)
  (evil-define-key 'insert org-mode-map [tab] 'org-metaright)
  (evil-define-key 'insert org-mode-map [S-tab] 'org-metaleft))

(with-eval-after-load 'telega
  ;; Emacs-mode by default in telega-chat-mode
  (push 'telega-chat-mode evil-emacs-state-modes))

(with-eval-after-load 'sqlite-mode
  (evil-define-key 'normal sqlite-mode-map (kbd "RET") 'sqlite-mode-list-data)
  (evil-define-key 'normal sqlite-mode-map (kbd "g c") 'sqlite-mode-list-columns)
  (evil-define-key 'normal sqlite-mode-map (kbd "g r") 'sqlite-mode-list-tables)
  (evil-define-key 'normal sqlite-mode-map (kbd "DEL") 'sqlite-mode-delete))

(provide 'prelude-evil)
