(require 'ansi-color)

(setq initial-scratch-message
      (with-temp-buffer
	(lisp-interaction-mode)
	(insert (shell-command-to-string "fortune"))
	(comment-region (point-min) (point-max))
	(ansi-color-apply-on-region (point-min) (point-max))
	(insert "\n;; Happy Hacking!\n;; Don't forget to check your calendar (C-c a)\n\n")
	(buffer-string)))
