;;; -*- lexical-binding: t; -*-
;; run benchmark-init/show-durations-tree after init

(use-package benchmark-init
  :disabled
  :init
  (add-hook 'after-init #'(lambda () (benchmark-init/deactivate)))
  (benchmark-init/activate))

(provide 'prelude-benchmark)
