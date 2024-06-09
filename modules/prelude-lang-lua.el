(use-package lua-mode
  :bind (:map lua-mode-map
              ("C-c C-z" . run-lua)
              ;; ("C-c C-c" . )
              )
  )

(provide 'prelude-lang-lua)
