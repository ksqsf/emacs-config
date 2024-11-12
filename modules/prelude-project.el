;;; -*- lexical-binding: t; -*-
;; Deal with the concept of `Project'

(use-package project
  :ensure nil
  :bind (:map project-prefix-map
              ("s" . project-vterm))
  :custom
  (project-buffers-viewer 'project-list-buffers-buffer-ibuffer)
  (project-switch-use-entire-map nil)
  (project-switch-commands 'project-find-file)

  :config
  (require 'projection)
  (require 'projection-multi)

  ;; vterm integraion
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
  (defun project-vterm ()
    (interactive)
    (require 'vterm)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if vterm-buffer
          (pop-to-buffer vterm-buffer current-prefix-arg)
        (vterm current-prefix-arg)))))

(use-package projection
  :hook (after-init . global-projection-hook-mode)
  :bind-keymap
  ("C-x P" . projection-map)
  :bind (:map project-prefix-map
              ("a" . projection-find-other-file))
  :config
  (setq projection-project-type-rime-config
    (projection-type
     :name 'rime-config
     :predicate (defun projection-rime-config-project-p ()
                  (and (file-exists-p "default.yaml")))
     :configure ""
     :build ""
     :test ""
     :run ""
     :install ""
     :compile-multi-targets
     '(("rime:squirrel:deploy" . "\"/Library/Input Methods/Squirrel.app/Contents/MacOS/Squirrel\" --reload")
       ("rime:f5m:copy and deploy" . "./fcitx.sh && \"/Library/Input Methods/Fcitx5.app/Contents/bin/fcitx5-curl\" /config/addon/rime/deploy -X POST -d '{}'")
       ("rime:f5m:deploy" . "\"/Library/Input Methods/Fcitx5.app/Contents/bin/fcitx5-curl\" /config/addon/rime/deploy -X POST -d '{}'"))))
  (add-to-list 'projection-project-types projection-project-type-rime-config))

(use-package projection-multi
  :bind (:map project-prefix-map
              ("RET" . projection-multi-compile)))

(use-package projectile
  :disabled
  ;; I'm really sad to let projectile go, but it does not handle remote files very well.
  ;; Currently, I will be experimenting with 

  :hook (after-init . projectile-mode)
  :diminish ""

  ;; Steal `C-x p' from project.el.
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind-keymap ("H-p" . projectile-command-map)

  :custom
  (projectile-auto-discover nil)
  (projectile-completion-system 'auto)
  (projectile-switch-project-action #'projectile-find-file)
  (projectile-current-project-on-switch 'move-to-end)
  (projectile-find-dir-includes-top-level t)
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)

  ;; Emulate project.el keybindings
  :bind (:map projectile-command-map
              ("f" . projectile-find-file)
              ("g" . projectile-ripgrep)
              ("s" . projectile-run-vterm-other-window)
              ("x x" . projectile-run-vterm)
              ("i" . projectile-install-project)
              ("I" . projectile-invalidate-cache)
              ;; ("s s" . projectile-ripgrep)
              ;; ("s a" . projectile-ag)
              )

  :config
  (add-to-list 'projectile-globally-ignored-files ".tags")

  ;; disable project detecion on remote files
  (advice-add 'projectile-project-root :around
              (lambda (orig &optional dir)
                (when (not (file-remote-p default-directory))
                  (funcall orig dir))))

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

  (add-to-list 'projectile-other-file-alist '("schema.yaml" "dict.yaml"))
  (add-to-list 'projectile-other-file-alist '("dict.yaml" "schema.yaml"))
  (projectile-register-project-type
   'rime
   '("default.yaml")
   :project-file "default.yaml"
   :configure ""
   :compile ""
   :run "\"/Library/Input Methods/Squirrel.app/Contents/MacOS/Squirrel\" --reload"
   :install ""
   :package "")

  :preface

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
            (t (error "Packages `ripgrep' and `rg' are not available")))))

  ;; PTerm related
  (defun projectile-run-pterm-other-window (&optional arg)
    "Invoke `pterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
    (interactive "P")
    (projectile--pterm arg 'other-window))

  (defun projectile-run-pterm (&optional arg)
    "Invoke `pterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
    (interactive "P")
    (projectile--pterm arg))

  (defun projectile--pterm (&optional new-process other-window)
    "Invoke `vterm' in the project's root.

Use argument NEW-PROCESS to indicate creation of a new process instead.
Use argument OTHER-WINDOW to indentation whether the buffer should
be displayed in a different window.

Switch to the project specific term buffer if it already exists."
    (let* ((project (projectile-acquire-root))
           (buffer (projectile-generate-process-name "pterm" new-process project)))
      (if (buffer-live-p (get-buffer buffer))
          (if other-window
              (switch-to-buffer-other-window buffer)
            (switch-to-buffer buffer))
        (projectile-with-default-dir project
          (progn
            (if other-window
                (pterm-other-window buffer)
              (pterm buffer))
            (rename-buffer (concat "*term: " (projectile-project-name) "*") t)))))))

(provide 'prelude-project)
