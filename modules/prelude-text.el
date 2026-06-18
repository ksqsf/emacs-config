;; -*- lexical-binding: t; -*-

(use-package markdown-ts-mode
  :if (version<= "31" emacs-version)
  :mode ("\\.md\\'" . markdown-ts-mode)
  :config
  (setq eglot-documentation-renderer 'markdown-ts-mode))

(provide 'prelude-text)
