;;; -*- lexical-binding: t; -*-

(use-package flx)
(use-package amx)

(use-package ivy
  :commands (ivy-mode)
  :bind (("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :init
  (add-hook 'after-init-hook #'ivy-mode)
  :config
  (setq ivy-extra-directories '("./"))
  (setq ivy-use-virtual-buffers 'recentf))

(use-package counsel
  :bind (("C-c c" . counsel-compile)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c L" . counsel-git-log)
         ("C-c k" . counsel-rg)
         ("C-x C-l" . counsel-locate)
         ("C-c J" . counsel-file-jump)
         ("C-c C-o" . counsel-outline))
  :init
  (add-hook 'after-init-hook #'counsel-mode))

(use-package ivy-hydra
  :after ivy)

(use-package ivy-posframe
  :disabled
  :after ivy
  :config
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
  (ivy-posframe-mode 1))

(provide 'prelude-completion)
