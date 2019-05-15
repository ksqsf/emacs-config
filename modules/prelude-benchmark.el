;; run benchmark-init/show-durations-tree after init

(add-to-list 'load-path (expand-file-name "elpa/benchmark-init-20150905.938" user-emacs-directory))
(require 'benchmark-init-modes)
(require 'benchmark-init)
(benchmark-init/activate)

(provide 'prelude-benchmark)
