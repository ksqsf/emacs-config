;; -*- lexical-binding: t; -*-

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("jinja" . "\\.html\\'"))))

(provide 'prelude-lang-web)
