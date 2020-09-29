(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-engines-alist
        '(("jinja" . "\\.html\\'"))))

(provide 'prelude-lang-web)
