;;; -*- lexical-binding: t; -*-

(use-package intero
  :commands (haskell-mode intero-mode)
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . intero-mode))

(provide 'prelude-lang-haskell)
