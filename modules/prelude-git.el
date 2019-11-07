;;; -*- lexical-binding: t; -*-

(use-package magit
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magit)

(provide 'prelude-git)
