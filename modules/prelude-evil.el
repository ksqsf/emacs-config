;;; -*- lexical-binding: t; -*-

;; Currently, I consider this an ``addition'' -- my config still works if you disable this.
;; Therefore, evil is loaded AFTER other modules, and other modules should not depend on the presence of evil.

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
               ("U" . evil-redo)))
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

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
(with-eval-after-load 'evil
  (with-eval-after-load 'corfu
    (add-hook 'evil-normal-state-entry-hook
              (lambda ()
                (corfu-quit)))))

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

(provide 'prelude-evil)
