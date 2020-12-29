;;; -*- lexical-binding: t; -*-
;;; things that don't fit elsewhere
(use-package wakatime-mode
  :commands (global-wakatime-mode)
  :defer 3
  :config
  (global-wakatime-mode t))

;; eshell sends notifications when commands finished
;; stolen from https://blog.hoetzel.info/post/eshell-notifications/
(use-package alert
  :after (eshell)
  :config
  (when (fboundp 'mac-do-applescript)
    (define-advice alert-osx-notifier-notify (:override (info))
      (mac-do-applescript (format "display notification %S with title %S"
				  (alert-encode-string (plist-get info :message))
				  (alert-encode-string (plist-get info :title))))
      (alert-message-notify info)))

  (defun eshell-command-alert (process status)
    (let* ((cmd (process-command process))
	   (buffer (process-buffer process))
	   (msg (format "%s: %s" (mapconcat 'identity cmd " ") status)))
      (if (string-prefix-p "finished" status)
	  (alert msg :buffer buffer :severity 'normal)
	(alert msg :buffer buffer :severity 'urgent))))

  (alert-add-rule :status '(buried)
		  :mode 'eshell-mode
		  :style (if *is-a-mac*
			     'osx-notifier
			   'notifications))

  (add-hook 'eshell-kill-hook #'eshell-command-alert))

;; cabon now sh
(use-package carbon-now-sh
  :commands (carbon-now-sh))

;; elfeed
(use-package elfeed
  :commands (elfeed))

;; speed-type
(use-package speed-type
  :commands (speed-type-text speed-type-buffer))

;; telega
(use-package telega
  :commands (telega))

;; google translate
(use-package google-translate
  :commands (google-translate-buffer google-translate-at-point)
  :config
  (define-advice google-translate-json-suggestion (:override (json))
    (let ((info (aref json 7)))
      (if (and info (> (length info) 0))
          (aref info 1)
        nil))))

;; org-roam
(use-package org-roam
  :defer t
  :custom (org-roam-directory "~/Documents/Roam")
  :bind (("C-c n l" . org-roam)
         ("C-c n t" . org-roam-dailies-today)
         ("C-c n f" . org-roam-find-file)
         ("C-c n j" . org-roam-jump-to-index)
         ("C-c n b" . org-roam-switch-to-buffer)
         ("C-c n g" . org-roam-graph)
         :map org-mode-map
         (("C-c n i" . org-roam-insert)
          ("C-c n I" . org-roam-insert-immediate))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Documents/Roam"))

(use-package org-roam-server
  :defer t
  :after org-roam
  :commands (org-roam-server-mode)
  :bind (:map org-roam-mode-map
              (("C-c n s" . org-roam-server)))
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  (require 'org-protocol)
  (require 'org-roam-protocol))

(use-package treemacs
  :bind (("C-c t" . treemacs)
         ("M-0" . treemacs-select-window))
  :commands (treemacs)
  :config

  ;; Add the name of the current workspace to the mode line
  (defun prelude-treemacs-mode-line ()
    '(" Treemacs " (:eval)))
  (setq treemacs-user-mode-line-format '(" Treemacs [" (:eval (treemacs-workspace->name (treemacs-current-workspace))) "] "))

  ;;
  (setq prelude-treemacs-default-workspace "Default")

  (defun -treemacs-get-workspace (name)
    (let ((workspaces (->> treemacs--workspaces
                           (--reject (eq it (treemacs-current-workspace)))
                           (--map (cons (treemacs-workspace->name it) it)))))
      (cdr (--first (string= (car it) name) workspaces))))

  (defun -treemacs-create-workspace (name)
    (treemacs-block
     (treemacs-return-if (treemacs--is-name-invalid? name)
       `(invalid-name ,name))
     (-when-let (ws (--first (string= name (treemacs-workspace->name it))
                             treemacs--workspaces))
       (treemacs-return `(duplicate-name ,ws)))
     (-let [workspace (treemacs-workspace->create! :name name)]
       (add-to-list 'treemacs--workspaces workspace :append)
       (treemacs--persist)
       (run-hook-with-args 'treemacs-create-workspace-functions workspace)
       `(success ,workspace))))

  (defun -treemacs-get-or-create-workspace (name)
    (or (--first (string= name (treemacs-workspace->name it))
                 treemacs--workspaces)
        (let (res (-treemacs-create-workspace name))
          (if (equal (car res) 'success)
              (cdr res)
            (error "Couldn't create workspace")))))

  (defun -treemacs-switch-to-workspace (ws)
    (setf (treemacs-current-workspace) ws)
    (treemacs--invalidate-buffer-project-cache)
    (treemacs--rerender-after-workspace-change)
    (run-hooks 'treemacs-switch-workspace-hook))

  (defun -treemacs-which-workspace ()
    "Which workspace does the current file belong to?"
    (--first (treemacs-is-path (buffer-file-name) :in-workspace it) (treemacs-workspaces)))

  ;; (setq current-file-ws (-treemacs-which-workspace))
  ;; (setq default-ws (-treemacs-get-or-create-workspace prelude-treemacs-default-workspace))

  ;;   (defun -treemacs-add-project-to-workspace-or-switch ()
  ;;     "Add the current project to the default workspace, or locate it if it's already known in a workspace.

  ;; The default workspace is specificied by `prelude-treemacs-default-workspace'"
  ;;     (let (current-file-ws (-treemacs-which-workspace))
  ;;       (if current-file-ws
  ;;           (progn
  ;;             (-treemacs-switch-to-workspace current-file-ws)
  ;;             (treemacs--follow))
  ;;         (let ((default-ws (-treemacs-get-or-create-workspace prelude-treemacs-default-workspace)))
  ;;           (message "I'm here!")
  ;;           (-treemacs-switch-to-workspace default-ws)
  ;;           (treemacs-display-current-project-exclusively)))))

  ;; (defun -treemacs-locate-project-if-in-workspace ()
  ;;   (let ((ws (-treemacs-which-workspace)))
  ;;     (when (and (not (null ws))
  ;;                (not (eq ws (treemacs-current-workspace))))
  ;;       (-treemacs-switch-to-workspace ws))))

  ;; (add-hook 'treemacs-mode-hook #'variable-pitch-mode)
  ;; (add-hook 'projectile-after-switch-project-hook #'-treemacs-add-project-to-workspace-or-switch)
  ;; (add-hook 'buffer-list-update-hook #'-treemacs-locate-project-if-in-workspace)

  (use-package treemacs-projectile))

(use-package pdf-tools
  :init
  (pdf-loader-install))

(provide 'prelude-apps)
