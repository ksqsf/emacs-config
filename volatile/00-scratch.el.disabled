(defun refresh-scratch ()
  "Refresh the *scratch* buffer with a fortune cookie."
  (interactive)
  (make-process :name "fortune"
                ;; https://github.com/ksqsf/fortune-monika
                :command '("fortune" "monika")
                :filter
                (lambda (process output)
                  (with-current-buffer "*scratch*"
                    (fundamental-mode)
                    (erase-buffer)
                    (insert output)
                    (insert "\nHappy Hacking!\nDon't forget to check your calendar (C-c a)\n\n")
                    (lisp-interaction-mode)
                    (comment-region (point-min) (point-max))
                    (require 'ansi-color)
                    (ansi-color-apply-on-region (point-min) (point-max))))))

(add-hook 'after-init-hook #'refresh-scratch)
