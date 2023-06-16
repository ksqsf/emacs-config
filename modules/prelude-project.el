;;; -*- lexical-binding: t; -*-
;; Deal with the concept of `Project'

(use-package projectile
  :hook (after-init . projectile-mode)
  :diminish ""

  ;; Steal `C-x p' from project.el.
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind-keymap ("H-p" . projectile-command-map)

  :custom
  (projectile-auto-discover nil)
  (projectile-completion-system 'auto)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-find-dir-includes-top-level t)
  (projectile-enable-caching nil)       ; Projectile will save a cache file, which is
                                        ; slow for large projects.

  ;; Emulate project.el keybindings
  :bind (:map projectile-command-map
              ("f" . projectile-find-file)
              ("g" . projectile-ripgrep)
              ("s" . projectile-run-vterm)
              ("x x" . projectile-run-vterm)
              ;; ("s s" . projectile-ripgrep)
              ;; ("s a" . projectile-ag)
              )

  :config

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
   :package "xmake package")

  ;; fix projectile bug
  (defun projectile-ripgrep (search-term &optional arg)
    "Run a ripgrep (rg) search with `SEARCH-TERM' at current project root.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on of the Emacs packages ripgrep or rg being
installed to work."
    (interactive
     (list (projectile--read-search-string-with-default
            (format "Ripgrep %ssearch for" (if current-prefix-arg "regexp " "")))
           current-prefix-arg))
    (let ((args (mapcar (lambda (val) (concat "--glob !" (shell-quote-argument val)))
                        (append projectile-globally-ignored-files
                                projectile-globally-ignored-directories))))
      ;; we rely on the external packages ripgrep and rg for the actual search
      ;;
      ;; first we check if we can load ripgrep
      (cond ((require 'ripgrep nil 'noerror)
             (ripgrep-regexp search-term
                             (projectile-acquire-root)
                             (if arg
                                 args
                               (cons "--fixed-strings --hidden" args))))
            ;; and then we try rg
            ((require 'rg nil 'noerror)
             (rg-run search-term
                     "*"                       ;; all files
                     (projectile-acquire-root)
                     (not arg)                 ;; literal search?
                     nil                       ;; no need to confirm
                     args))
            (t (error "Packages `ripgrep' and `rg' are not available"))))))

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
