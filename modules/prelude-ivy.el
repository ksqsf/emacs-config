;;; -*- lexical-binding: t; -*-
;; Ivy and Counsel
(ensure-package 'flx)
(ensure-package 'ivy)
(ensure-package 'counsel)

(add-hook 'after-init-hook #'ivy-mode)
(add-hook 'after-init-hook #'counsel-mode)

(setq ivy-extra-directories '("./"))

(provide 'prelude-ivy)
