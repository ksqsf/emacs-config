;;; -*- lexical-binding: t; -*-

(use-package evil
  :hook (after-init . evil-mode)
  :config
  (setq evil-undo-system 'undo-redo))

(provide 'prelude-evil)
