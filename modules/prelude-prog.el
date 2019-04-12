;;; Configuration for programming needs.
;;; Some portions might be a standalone module.

(defun prelude--enter-prog ()
  "Common tasks before entering a `prog-mode'-derived major
mode."
  (hl-todo-mode t)               			    ; Highlight TODOs
  (company-mode t)                                          ; Auto complete
  )

(add-hook 'prog-mode-hook #'prelude--enter-prog)


;;; Company
(setq company-idle-delay 0.0)


;;; GDB
;; Refer to https://debbugs.gnu.org/cgi/bugreport.cgi?bug=33548
(setq gdb-mi-decode-strings 'utf-8)


(provide 'prelude-prog)
