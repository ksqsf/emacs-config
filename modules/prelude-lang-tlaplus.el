;;; -*- lexical-binding: t; -*-

(defun tla-insert-separator ()
  "Insert dashes as separator.

The number of dashes is determined by the MOUDLE line."
  (interactive)
  (let ((width (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "-+ ?MODULE")
                 (- (point-at-eol) (point-at-bol)))))
    (insert (make-string width ?-))))

(use-package tla-tools
  :quelpa (tla-tools :fetcher github :repo "mrc/tla-tools"))

(provide 'prelude-lang-tlaplus)
