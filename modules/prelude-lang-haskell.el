;;; -*- lexical-binding: t; -*-

(use-package intero
  :commands (haskell-mode intero-mode)
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . intero-mode))

;; (use-package haskell-mode
;;   :mode ("\\.hs\\'" . haskell-mode))

;; (use-package dante
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   (add-hook 'haskell-mode-hook 'dante-mode))

(provide 'prelude-lang-haskell)
