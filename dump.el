(load (expand-file-name "init.el"))

(setq +dump-exclude-packages '(telega vterm))
(setq +dump-include-packages '())

(defun +load-noerror (package)
  (message "Loading %s" (prin1 package))
  (ignore-errors (require package)))

(dolist (package +dump-include-packages)
  (+load-noerror package))

(dolist (package package-activated-list)
  (unless (member package +dump-exclude-packages)
    (+load-noerror package)))

(setq +dumped-load-path load-path)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
