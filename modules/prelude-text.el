;; -*- lexical-binding: t; -*-

(use-package markdown-ts-mode
  :if (version<= "31" emacs-version)
  :mode ("\\.md\\'" . markdown-ts-mode)
  :config
  (setq eglot-documentation-renderer 'markdown-ts-mode))

(use-package yaml-mode
  :iload (yaml-mode)
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package dockerfile-mode
  :mode ("Dockerfile.*\\'" . dockerfile-mode)
  :mode (".dockerfile\\'" . dockerfile-mode))

(provide 'prelude-text)
