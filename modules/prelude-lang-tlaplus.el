;;; -*- lexical-binding: t; -*-

(defun tla-insert-separator ()
  "Insert dashes as separator.

The number of dashes is determined by the MOUDLE line."
  (interactive)
  (let ((width (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "-+ ?MODULE")
                 (- (pos-eol) (pos-bol)))))
    (insert (make-string width ?-))))

;; NOTE: this does not work...
(use-package tla-tools
  :vc (:fetcher github :repo "mrc/tla-tools"))

(provide 'prelude-lang-tlaplus)
