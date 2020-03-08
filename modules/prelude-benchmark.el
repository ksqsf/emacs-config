;;; -*- lexical-binding: t; -*-
;; run benchmark-init/show-durations-tree after init

(use-package benchmark-init
  :init
  (add-hook 'after-init #'(lambda () (benchmark-init/deactivate)))
  :config
  (benchmark-init/activate))

(provide 'prelude-benchmark)
