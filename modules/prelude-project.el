;;; -*- lexical-binding: t; -*-
;; Deal with the concept of `Project'

(use-package projectile
  :hook (after-init . projectile-mode)
  ;; Steal `C-x p' from project.el.
  :bind-keymap ("C-x p" . projectile-command-map)

  :config
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching nil)
  (setq projectile-switch-project-action #'projectile-find-file)

  (define-key projectile-command-map (kbd "x x") #'projectile-run-vterm)

  ;; cmake
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake -Bbuild"
                                    :compile "cmake --build build"
                                    :test "cd build && ctest"))

(use-package project
  :defer t
  :config
  (defun project-shell ()
  "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists.

Modified to run vterm instead of shell."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-shell-name (project-prefixed-buffer-name "vterm"))
         (shell-buffer (get-buffer default-project-shell-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (pop-to-buffer-same-window shell-buffer)
      (vterm-other-window (generate-new-buffer-name default-project-shell-name))))))

(provide 'prelude-project)
