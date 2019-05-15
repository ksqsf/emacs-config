;;; Config for C/C++/Java/...

(cl-eval-when 'compile
  (require 'cc-mode))

(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(c-mode . "k&r")
			(c++-mode . "stroustrup")
			(other . "gnu")))

(defun prelude--enter-cc ()
  (yas-minor-mode)
  (define-key c-mode-map (kbd "<f5>") #'compile)
  (define-key c++-mode-map (kbd "<f5>") #'compile))

(add-hook 'c-mode-common-hook #'prelude--enter-cc)
(add-hook 'c++-mode-hook #'prelude--enter-cc)

(provide 'prelude-lang-cc)
