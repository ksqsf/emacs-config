;;; -*- lexical-binding: t; -*-
(ensure-package 'elpy)

;; Prefer Python 3
(setq doom-modeline-env-python-executable "python3")
(setq python-shell-interpreter "python3")
(setq elpy-rpc-python-command "python3"
      elpy-get-info-from-shell t)

(with-eval-after-load "python"
  (elpy-enable))

(add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (company-mode t)))

(provide 'prelude-lang-python)
