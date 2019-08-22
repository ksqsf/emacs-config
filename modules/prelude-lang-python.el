;;; -*- lexical-binding: t; -*-

(ensure-package 'elpy)

(defun prelude/setup-python ()
  ;; Prefer Python 3
  (setq doom-modeline-env-python-executable "python3")
  (setq python-shell-interpreter "python3")
  (setq elpy-rpc-python-command "python3"
        elpy-get-info-from-shell t)

  ;; enable elpy
  (elpy-enable)

  ;; my keys
  (define-key elpy-mode-map (kbd "C-c C-c") #'elpy-shell-send-region-or-buffer-and-go)
  (define-key python-mode-map (kbd "C-z C-z") #'elpy-shell-send-region-or-buffer-and-go)
  (define-key python-mode-map (kbd "C-M-x") #'elpy-shell-send-defun-and-go))

(with-eval-after-load "python"
  (prelude/setup-python))

(add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (company-mode t)))

(provide 'prelude-lang-python)
