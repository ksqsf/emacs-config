(make-process :name "fortune"
              :command '("fortune")
              :filter
              (lambda (process output)
                (with-current-buffer "*scratch*"
                  (erase-buffer)
                  (insert output)
                  (insert "\nHappy Hacking!\nDon't forget to check your calendar (C-c a)\n\n")
                  (lisp-interaction-mode)
                  (comment-region (point-min) (point-max))
                  (require 'ansi-color)
                  (ansi-color-apply-on-region (point-min) (point-max)))))
