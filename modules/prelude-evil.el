;; -*- lexical-binding: t; -*-

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

;; Quit corfu on Escape
(with-eval-after-load 'evil
  (with-eval-after-load 'corfu
    (add-hook 'evil-normal-state-entry-hook
              (lambda ()
                (corfu-quit)))))

;; Leader key
(use-package general
  :init
  (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
  :config
  (general-evil-setup)
  (general-nmap
   :prefix "SPC"
   :prefix-map '+leader-map
   "SPC" 'execute-extended-command
   ;; buffer
   "bi" 'ibuffer
   "bb" 'switch-to-buffer
   "bk" 'kill-this-buffer
   "br" 'bury-buffer
   "bc" 'clone-indirect-buffer
   "bd" 'delete-trailing-whitespace
   "bu" 'revert-buffer
   ;; projectile
   "pp" 'projectile-switch-project
   "ps" 'projectile-vterm
   "pf" 'projectile-find-file
   "pb" 'projectile-ibuffer
   "pk" 'projectile-kill-buffers
   "pg" 'projectile-grep
   "pa" 'projectile-find-other-file-other-window
   ;; gpt
   "gg" 'gptel
   "gm" 'gptel-menu
   "gp" 'gptel-system-prompt
   "gs" 'gptel-send
   ))

(evil-define-key 'normal 'global (kbd "gh") 'previous-buffer)
(evil-define-key 'normal 'global (kbd "gl") 'next-buffer)
(evil-define-key 'normal haskell-mode-map (kbd "gz") 'haskell-interactive-switch)
(evil-define-key 'motion 'global (kbd "gs") 'gptel-send)
(evil-define-key 'visual 'global (kbd "gs") 'gptel-send)

(provide 'prelude-evil)
