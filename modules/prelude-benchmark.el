;;; -*- lexical-binding: t; -*-
;; run benchmark-init/show-durations-tree after init

(use-package benchmark-init
  :demand t
  :config (benchmark-init/activate)
  :hook   (after-init . benchmark-init/deactivate))

(provide 'prelude-benchmark)
