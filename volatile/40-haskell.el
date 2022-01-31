(setq +enable-structured-haskell-mode nil)
;; Let me practise first... I cannot use it fluently yet.
(when +enable-structured-haskell-mode
  (with-eval-after-load 'haskell-mode
    (add-to-list 'load-path (expand-file-name "~/src/Clone/structured-haskell-mode/elisp"))
    (require 'shm)
    (add-hook 'haskell-mode-hook 'structured-haskell-mode)))
