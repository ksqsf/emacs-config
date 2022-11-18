;;; -*- lexical-binding: t; -*-

(use-package magit
  :defer t
  :bind (("C-c g" . magit-file-dispatch)))

(use-package forge
  :after magit
  :defer t)

(provide 'prelude-git)
