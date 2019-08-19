;;; -*- lexical-binding: t; -*-
;;; things that don't fit elsewhere

(ensure-package 'wakatime-mode)
(global-wakatime-mode t)

;; eshell sends notifications when commands finished
;; stolen from https://blog.hoetzel.info/post/eshell-notifications/
(ensure-package 'alert)
(require 'alert)

(when (fboundp 'mac-do-applescript)
  (defun alert-osx-notifier-notify (info)
    "Patched to make use of Emacs MacPort's native AppleScript
  capability."
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

(add-hook 'eshell-kill-hook #'eshell-command-alert)

(provide 'prelude-other)
