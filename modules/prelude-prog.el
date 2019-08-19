;;; -*- lexical-binding: t; -*-
;;; Configuration for programming needs.
;;; Some portions might be a standalone module.

(ensure-package 'company)
(ensure-package 'yasnippet)
(ensure-package 'hl-todo)
(ensure-package 'rainbow-delimiters)

(setq-default indent-tabs-mode nil)

(defun prelude/enter-prog ()
  "Common tasks before entering a `prog-mode'-derived major
mode."
  (hl-todo-mode t)               			    ; Highlight TODOs
  (company-mode t)                                          ; Auto complete
  (rainbow-delimiters-mode t)
  (define-key prog-mode-map (kbd "C-c C-j") #'imenu)
  )

(add-hook 'prog-mode-hook #'prelude/enter-prog)


;;; Company
(setq company-idle-delay 0.3)


;;; GDB
;; Refer to https://debbugs.gnu.org/cgi/bugreport.cgi?bug=33548
(setq gdb-mi-decode-strings 'utf-8)


(provide 'prelude-prog)
