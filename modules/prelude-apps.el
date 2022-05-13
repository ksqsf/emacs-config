;;; -*- lexical-binding: t; -*-
;;; things that don't fit elsewhere
(use-package wakatime-mode
  :diminish ""
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
		  :style (if k|mac
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

  (use-package treemacs-projectile)

  (use-package treemacs-all-the-icons
    :config
    (treemacs-load-theme "all-the-icons")))

(use-package pdf-tools
  :disabled
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)

  (use-package pdf-continuous-scroll-mode
    :disabled                           ; This mode barely does anything useful
    :quelpa (pdf-continuous-scroll-mode :fetcher github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
    :hook (pdf-view-mode . pdf-continuous-scroll-mode)))

(use-package dash-at-point
  :commands (dash-at-point)
  :bind (("C-c C-d" . dash-at-point)))

(use-package eaf
  :disabled ;; Does not work...
  :commands (eaf-open)
  :load-path "lisp/emacs-application-framework"
  :config
  (require 'eaf-demo)
  (require 'eaf-file-sender)
  (require 'eaf-camera)
  (require 'eaf-browser)
  (require 'eaf-file-browser)
  (require 'eaf-file-manager)
  (require 'eaf-airshare))

(use-package go-translate
  :commands (gts-do-translate)
  :config
  (setq gts-translate-list '(("en" "zh" "jp")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine) (gts-google-rpc-engine)
                        (gts-bing-engine) )
         :render (gts-buffer-render))))

(provide 'prelude-apps)
