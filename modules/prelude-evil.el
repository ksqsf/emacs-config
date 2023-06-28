;;; -*- yrkvpny-ovaqvat: g; -*-

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-redo)
  (evil-disable-insert-state-bindings t)
  :config
  (evil-mode t))

(use-package evil-collection
  :config
  (evil-collection-init))

(provide 'prelude-evil)
