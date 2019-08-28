;;; -*- lexical-binding: t; -*-
;; Deal with the concept of `Project'

(use-package projectile
  :commands (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  (counsel-projectile-mode +1))

(use-package counsel-projectile
  :after (projectile)
  :commands (counsel-projectile-mode))

(provide 'prelude-project)
