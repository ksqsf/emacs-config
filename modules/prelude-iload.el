;;; incremental loading -*- lexical-binding:t; -*-

;; inspired by https://github.com/Elilif/.elemacs/core-incremental-loading.el

;; usage: (+iload '(feature1 feature2 ...))
;; then each of them will be loaded incrementally after emacs startup.

(defvar +iload-list nil
  "The list of features to be loaded incrementally.")
(defvar +iload-timer nil
  "The timer that runs the incremental loading process.")
(defvar +iload-delay 1.0
  "The delay between each incremental load.")

(defun +iload (features)
  "Register FEATURE to be loaded incrementally.

Each feature in FEATURES will be loaded one by one in the order.
However, the loading order of multiple calls to `+iload' is not
guaranteed."
  (dolist (feature (reverse features))
    (push feature +iload-list)))

(defun +iload-do-load-one ()
  "Load one feature from `+iload-list', and remove it from the list.

Returns t if there are more features to load, nil otherwise."
  (when-let ((feature (pop +iload-list)))
    ;; (message "Incrementally loading %s..." feature)
    (require feature nil t)
    (not (null +iload-list))))

(defun +iload-start ()
  "Start the incremental loading process."
  (when +iload-timer
    (cancel-timer +iload-timer))
  (setq +iload-timer
        (run-with-idle-timer +iload-delay t
                             #'(lambda ()
                                 (or (+iload-do-load-one)
                                     (prog1 (cancel-timer +iload-timer)
                                       (setq +iload-timer nil)))))))

(defmacro iload (&rest features)
  "A slightly easier wrapper over `+iload'.

Example: (iload org-macs org-compat org)"
  `(+iload (quote ,features)))

(add-hook 'emacs-startup-hook #'+iload-start)

(provide 'prelude-iload)
