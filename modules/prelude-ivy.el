;;; -*- lexical-binding: t; -*-
;; Ivy and Counsel
(ensure-package 'flx)
(ensure-package 'ivy)
(ensure-package 'counsel)

(ivy-mode 1)
(counsel-mode 1)

(setq ivy-extra-directories '("./"))

(provide 'prelude-ivy)
