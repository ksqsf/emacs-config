;;; -*- lexical-binding: t; -*-
;; Tuareg for OCaml

(use-package tuareg
  :mode ("\\.ml[4ilpy]?$" . tuareg-mode))

(use-package dune :after tuareg)

(use-package utop
  :after tuareg
  :bind (:map tuareg-mode-map)
  :when (executable-find "utop"))

(use-package merlin
  :hook (tuareg-mode . merlin-mode))

(provide 'prelude-lang-ml)
