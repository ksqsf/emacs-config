(use-package zig-mode
  :mode ("\\.zig\\'")
  :hook (zig-mode . k|lsp-ensure)
  ;; lsp server: zls.
  )

(provide 'prelude-lang-zig)
