;;; This is a base for all programming needs.

(defun prelude--enter-prog ()
  "Common tasks before entering a `prog-mode'-derived major
mode."
  (hl-todo-mode t)               			    ; Highlight TODOs
  (company-mode t)                                          ; Auto complete
  )

(add-hook 'prog-mode-hook #'prelude--enter-prog)

(provide 'prelude-prog)
