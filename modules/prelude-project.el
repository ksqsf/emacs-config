;; Deal with the concept of `Project'

(ensure-package 'projectile)
(ensure-package 'counsel-projectile)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)
(counsel-projectile-mode +1)

(provide 'prelude-project)
