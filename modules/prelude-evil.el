;; -*- lexical-binding: t; -*-

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
   ;; projectile
   "pp" 'projectile-switch-project
   "ps" 'projectile-vterm
   "pf" 'projectile-find-file
   "pb" 'projectile-ibuffer
   "pk" 'projectile-kill-buffers
   "pg" 'projectile-grep
   "pa" 'projectile-find-other-file-other-window
   ;; vc
   "v=" 'vc-diff
   "vg" 'magit
   ))

(provide 'prelude-evil)
