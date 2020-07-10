;;; -*- lexical-binding: t; -*-
;;; things that don't fit elsewhere
(use-package wakatime-mode
  :commands (global-wakatime-mode)
  :defer 1
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

;; stack exchange
(use-package sx)

;; telega
(use-package telega
  :commands (telega)
  :config
  )

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
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory "~/Documents/Roam")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n t" . org-roam-dailies-today)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))))

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
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(provide 'prelude-apps)
