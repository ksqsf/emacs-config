;;; -*- lexical-binding: t; -*-
;;; Config for C/C++/Java/...

(defvar +c-newline-and-indent-regexp "\\s)")

(defun +c-newline-and-indent ()
  "Open one more line when the next char is a closing paren.

The exact behavior can be controlled by
`c-newline-and-indent-regexp'.

This command relies on `c-indent-line' so it only works in CC
Mode."
  (interactive)
  (newline)
  (save-excursion
    (when (looking-at-p +c-newline-and-indent-regexp)
      (newline)
      (c-indent-line)))
  (c-indent-line))

(use-package cc-mode
  :ensure nil
  :defer t
  :config

  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "k&r")
                          (c++-mode . "stroustrup")
                          (other . "gnu")))
  (add-hook 'c-mode-common-hook #'k|lsp-ensure)
  ;; (add-hook 'c-mode-common-hook #'company-mode)
  (add-hook 'c-mode-common-hook #'subword-mode)
  ;; (add-hook 'c-mode-common-hook #'smartparens-mode)
  (add-hook 'c-mode-common-hook #'(lambda () (rainbow-mode 0)))
  (define-key c-mode-map (kbd "<f5>") #'compile)
  (define-key c++-mode-map (kbd "<f5>") #'compile)
  (define-key c-mode-base-map (kbd "`") (k|double-tap-to-insert ?\"))
  (define-key c-mode-base-map (kbd "RET") #'+c-newline-and-indent)

  (use-package ccls
    :disabled ;; default to clangd
    :config
    (setq ccls-sem-highlight-method 'font-lock)
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '(c++-mode . ("ccls")))
      (add-to-list 'eglot-server-programs '(c-mode . ("ccls"))))))

(use-package modern-cpp-font-lock
  :hook ((c++-mode . modern-c++-font-lock-mode)))

;;; Styles
(with-eval-after-load 'cc-mode
  (c-add-style "rime"
               '("gnu"
                 (c-offsets-alist
                  (innamespace . 0)
                  (access-label . -1)
                  (inclass . 2)))))

;;; ObjC++
;;; There seems to be no dedicated major mode for ObjC++. Just use objc-mode.
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(provide 'prelude-lang-cc)
