;;; -*- lexical-binding: t; -*-
;; Deal with the concept of `Project'

(use-package projectile
  :hook (after-init . projectile-mode)

  ;; Steal `C-x p' from project.el.
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind-keymap ("H-p" . projectile-command-map)

  :custom
  (projectile-completion-system 'auto)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-find-dir-includes-top-level t)
  (projectile-enable-caching nil)  ; Projectile will save a cache file, which is
                                   ; slow for large projects.

  :config

  (define-key projectile-command-map (kbd "x x") #'projectile-run-vterm)
  (define-key projectile-command-map (kbd "s s") #'projectile-ripgrep)
  (define-key projectile-command-map (kbd "s a") #'projectile-ag)

  ;; Support for Citre (a u-ctags frontend).
  (advice-add 'projectile-regenerate-tags :around
              (lambda (orig &rest args)
                (if citre-mode
                    (citre-update-this-tags-file)
                  (apply orig args))))

  ;; Register project types
  (projectile-register-project-type
   'xmake
   '("xmake.lua")
   :configure "xmake config"
   :compile "xmake build"
   :run "xmake run"
   :install "xmake install"
   :package "xmake package"))

(use-package project
  :defer t
  :ensure nil
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
