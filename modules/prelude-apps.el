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

(provide 'prelude-apps)
