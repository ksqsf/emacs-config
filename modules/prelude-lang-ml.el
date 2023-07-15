;;; -*- lexical-binding: t; -*-
;; Tuareg for OCaml

(use-package tuareg
  :mode ("\\.ml[4ilpy]?$" . tuareg-mode))

(use-package dune :after tuareg)

;; Requires: opam install utop
(use-package utop
  :after tuareg
  :bind (:map tuareg-mode-map)
  :when (executable-find "utop"))

;; Requires: opam install merlin
(use-package merlin
  :hook (tuareg-mode . merlin-mode)
  :when (executable-find "ocamlmerlin"))

(provide 'prelude-lang-ml)
