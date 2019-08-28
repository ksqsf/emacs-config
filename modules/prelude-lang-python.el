;;; -*- lexical-binding: t; -*-

(use-package python
  :defer t
  :init
  (setq doom-modeline-env-python-executable "python3")
  (setq python-shell-interpreter "python3")
  :config
  (add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (company-mode t))))

(use-package elpy
  :after (python)
  :hook (python-mode . elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3"
        elpy-get-info-from-shell t)
  (define-key elpy-mode-map (kbd "C-c C-c") #'elpy-shell-send-region-or-buffer-and-go)
  (define-key python-mode-map (kbd "C-z C-z") #'elpy-shell-send-region-or-buffer-and-go)
  (define-key python-mode-map (kbd "C-M-x") #'elpy-shell-send-defun-and-go))

(provide 'prelude-lang-python)
