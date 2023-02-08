;;; -*- lexical-binding: t; -*-

(use-package evil
  :hook (after-init . evil-mode)
  :config
  (setq evil-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'prelude-evil)
