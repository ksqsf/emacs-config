;;; -*- lexical-binding: t; -*-
;;; things that don't fit elsewhere
(use-package wakatime-mode
  ;; :when (not (eq system-type 'darwin))  ; wakatime can cause emacs to hang on macos
  :diminish ""
  :commands (global-wakatime-mode)
  :defer 3
  :config
  (global-wakatime-mode t))

;; eshell sends notifications when commands finished
;; stolen from https://blog.hoetzel.info/post/eshell-notifications/
(use-package alert
  :after (eshell)
  :commands (eshell-command-alert)
  :config
  (when (fboundp 'mac-do-applescript)
    (define-advice alert-osx-notifier-notify (:override (info))
      (mac-do-applescript (format "display notification %S with title %S"
				  (alert-encode-string (plist-get info :message))
				  (alert-encode-string (plist-get info :title))))
      (alert-message-notify info)))

  (alert-add-rule :status '(buried)
		  :mode 'eshell-mode
		  :style (if k|mac
			     'osx-notifier
			   'notifications))

  (defun eshell-command-alert (process status)
    (let* ((cmd (process-command process))
	   (buffer (process-buffer process))
	   (msg (format "%s: %s" (mapconcat 'identity cmd " ") status)))
      (if (string-prefix-p "finished" status)
	  (alert msg :buffer buffer :severity 'normal)
	(alert msg :buffer buffer :severity 'urgent))))
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
  :commands (telega)
  :config
  (setq telega-avatar-workaround-gaps-for '(return t)))

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

  (setq treemacs-is-never-other-window t)

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
        (let ((res (-treemacs-create-workspace name)))
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

  (use-package treemacs-projectile
    :after (projectile)
    :demand t)

  (use-package treemacs-tab-bar
    :after (tab-bar)
    :demand t)

  (use-package treemacs-nerd-icons
    :demand t
    :config
    (treemacs-load-theme "nerd-icons"))

  (use-package treemacs-persp
    :after (persp-mode)
    :demand t))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)

  (use-package pdf-continuous-scroll-mode
    :disabled                           ; This mode barely does anything useful
    :vc (:fetcher github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
    :hook (pdf-view-mode . pdf-continuous-scroll-mode)))



(use-package dash-at-point
  :commands (dash-at-point)
  :preface
  :bind (("M-g M-d" . dash-at-point)
         ("M-g d" . dash-at-point))
  :config
  (with-eval-after-load 'embark
    (define-key embark-symbol-map (kbd "d") 'dash-at-point)))

;; epub reader
(use-package nov
  :commands (nov-mode))

;; debbugs
(use-package debbugs
  :defer t)

(use-package ledger-mode
  :defer t)

;; world-clock
(setq world-clock-list
      '(("Asia/Chongqing" "China")
        ("Asia/Tokyo" "Japan")
        ("Europe/Berlin" "Germany")
        ("Europe/Paris" "France")
        ("America/New_York" "New York")
        ("America/Los_Angeles" "Los Angeles")))

;; beancount
(use-package beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :bind ( :map beancount-mode-map
          ("C-c C-n" . outline-next-visible-heading)
          ("C-c C-p" . outline-previous-visible-heading)))

;; elfeed
(use-package elfeed)
(use-package elfeed-summarize
  :after elfeed
  :config
  (setq elfeed-summarize-llm-provider +llm-small-model)
  (setq elfeed-feeds
        '(("https://ksqsf.moe/atom.xml" blog)
          ("https://mlzeng.com/index.xml" blog)
          ("https://blog.taoky.moe/feed.xml" blog)
          ("https://ibug.io/feed.xml" blog)
          ("https://sirius1242.github.io/feed.xml" blog)
          ("https://www.csslayer.info/wordpress/rss/" blog)
          ("http://farseerfc.me/feeds/atom.xml" blog)
          ("https://c-j.dev/atom.xml" blog)
          ("https://ring0.me/atom.xml" blog)
          ("https://voile.tech/atom.xml" blog)
          ("https://blog.xuesong.io/rss.xml" blog)
          ("https://konjacsource.github.io/rss.xml" blog)
          ("https://soaked.in/feed.xml" blog)
          ("https://lilting.ch/rss.xml" blog)
          ("https://www.qbitai.com/rss/" news ai)
          ("https://www.solidot.org/index.rss" news tech)
          ("https://hnrss.org/newest?points=150" news tech)
          ("https://lobste.rs/rss" news tech)
          ("https://www.quantamagazine.org/feed/")
          ("https://sspai.com/feed" tech)
          ("https://blog.rust-lang.org/feed.xml" tech rust)
          ("https://rss.lilydjwg.me/zhihuzhuanlan/c_1749542169038483456" cs)
          ("https://rss.lilydjwg.me/zhihuzhuanlan/c_1736814172640501761" cs)
          ("https://rss.lilydjwg.me/zhihuzhuanlan/marisa" cs)
          ("https://concurrencyfreaks.blogspot.com/atom.xml" cs)
          ("https://this-week-in-rust.org/atom.xml" news rust)
          ("https://haskellweekly.news/newsletter.atom" news haskell)
          ("https://plink.anyfeeder.com/weixin/caixinwang" news finance)
          ("https://finance.yahoo.com/news/rssindex" news finance)
          )))

;; emms
(use-package emms
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions '(emms-info-native)))

(provide 'prelude-apps)
