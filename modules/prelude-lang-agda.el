(defcustom prelude-agda-mode-path nil
  "Specify the path to the executable 'agda-mode'."
  :group 'prelude
  :type 'string)

;; Load agda-mode only when 'agda-mode' can be found.
(when-let ((agda-mode-path (or prelude-agda-mode-path
                               (executable-find "agda-mode"))))
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string (concat agda-mode-path " locate")))))

(provide 'prelude-lang-agda)

