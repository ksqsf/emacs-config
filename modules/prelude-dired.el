;; Search file names when point is at a file name; Search unlimitedly
;; otherwise.
(setq dired-isearch-filenames 'dwim)

;; Intelligently guess the target directory.
(setq dired-dwim-target t)

(provide 'prelude-dired)
