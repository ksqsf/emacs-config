;; Deal with the concept of `Project'

(ensure-package 'projectile)
(ensure-package 'counsel-projectile)

(require 'projectile)
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
(projectile-mode +1)
(counsel-projectile-mode +1)

(provide 'prelude-project)
