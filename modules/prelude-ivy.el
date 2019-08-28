;;; -*- lexical-binding: t; -*-

(use-package flx)

(use-package ivy
  :commands (ivy-mode)
  :init
  (add-hook 'after-init-hook #'ivy-mode)
  :config
  (setq ivy-extra-directories '("./"))
  (setq ivy-use-virtual-buffers 'recentf))

(use-package counsel
  :commands (counsel-mode counsel-M-x counsel-compile)
  :bind (("M-x" . counsel-M-x))
  :init
  (add-hook 'after-init-hook #'counsel-mode)
  (defalias 'compile #'counsel-compile))

(provide 'prelude-ivy)
