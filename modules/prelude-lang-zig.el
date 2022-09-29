(use-package zig-mode
  :mode ("\\.zig\\'")
  :hook (zig-mode . k|lsp-ensure))

(provide 'prelude-lang-zig)
