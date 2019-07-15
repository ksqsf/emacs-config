;; Deal with the concept of `Project'
;;
;; find-file-in-project and project.el cannot make use of ido, so projectile here.

(ensure-package 'projectile)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(provide 'prelude-project)
