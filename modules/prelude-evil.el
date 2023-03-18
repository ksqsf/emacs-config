;;; -*- lexical-binding: t; -*-

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq evil-undo-system 'undo-redo)
  (evil-mode t))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(provide 'prelude-evil)
