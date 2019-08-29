;;; -*- lexical-binding: t; -*-

(use-package magit
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

(provide 'prelude-git)
