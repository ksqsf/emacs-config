;;; -*- lexical-binding: t; -*-
;; Deal with the concept of `Project'

(use-package projectile
  :hook (after-init . projectile-mode)

  ;; Steal `C-x p' from project.el.
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind-keymap ("H-p" . projectile-command-map)

  :config
  (setq projectile-completion-system 'auto)
  (setq projectile-enable-caching nil)
  (setq projectile-switch-project-action #'projectile-commander)
  (setq projectile-find-dir-includes-top-level t)

  (define-key projectile-command-map (kbd "x x") #'projectile-run-vterm)
  (define-key projectile-command-map (kbd "s s") #'projectile-ripgrep)
  (define-key projectile-command-map (kbd "s a") #'projectile-ag)

  ;; Support for Citre (a u-ctags frontend).
  (advice-add 'projectile-regenerate-tags :around
              (lambda (orig &rest args)
                (if citre-mode
                    (citre-update-this-tags-file)
                  (apply orig args)))))

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
      (vterm-other-window (generate-new-buffer-name default-project-shell-name)))))

  ;; project.el should respect .projectile files
  (defun k|project-override (dir)
    (let ((override (locate-dominating-file dir ".projectile")))
    (if override
      (cons 'vc override)
      nil)))
  (add-hook 'project-find-functions #'k|project-override))

(provide 'prelude-project)
