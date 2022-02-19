;;; -*- lexical-binding: t; -*-
;; run benchmark-init/show-durations-tree after init

(use-package benchmark-init
  :init
  (benchmark-init/activate)
  (add-hook 'after-init #'(lambda () (benchmark-init/deactivate)))
  )

(provide 'prelude-benchmark)
