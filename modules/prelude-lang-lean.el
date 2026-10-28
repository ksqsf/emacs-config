;;; -*- lexical-binding: t; -*-

(use-package nael
  :mode ("\\.lean\\'" . nael-mode)
  :config
  (use-package nael-lsp))

(provide 'prelude-lang-lean)
