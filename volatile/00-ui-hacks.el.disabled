;; Only works on GNOME
;; Prefer dark variant

(call-process-shell-command (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark -name \""
				    (cdr (assoc 'name (frame-parameters)))
				    "\""))
