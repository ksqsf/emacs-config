;;; -*- lexical-binding: t; -*-

(use-package lean4-mode
  :mode ("\\.lean\\'" . lean4-mode)
  :vc (:fetcher github :repo "leanprover/lean4-mode")
  :config
  ;; disable company in lean4 mode because we use corfu
  (add-hook 'lean4-mode #'(lambda () (company-mode -1))))

(provide 'prelude-lang-lean)
