;;; -*- lexical-binding: t; -*-
;; Deal with the concept of `Project'

(use-package projectile
  :commands (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system
        (cond ((eq prelude-completion-framework 'ido-only) 'ido)
              ((eq prelude-completion-framework 'ivy+counsel) 'ivy)
              (t 'default)))
  (setq projectile-enable-caching nil)
  (setq projectile-switch-project-action #'projectile-find-file)
  (projectile-mode +1)
  ;; (counsel-projectile-mode +1)

  (define-key projectile-command-map (kbd "x x") #'projectile-run-vterm)

  ;; cmake
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake -Bbuild"
                                    :compile "cmake --build build"
                                    :test "cd build && ctest"))

(use-package counsel-projectile
  :after (projectile)
  :commands (counsel-projectile-mode))

(provide 'prelude-project)
