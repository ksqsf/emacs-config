;;; -*- lexical-binding: t; -*-
(ensure-package 'intero)

(defun prelude/setup-haskell ()
  (add-hook 'haskell-mode-hook 'intero-mode))

(eval-after-load 'haskell-mode
  (prelude/setup-haskell))

(provide 'prelude-lang-haskell)
