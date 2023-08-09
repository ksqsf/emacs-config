;;; incremental loading -*- lexical-binding:t; -*-
;; inspired by https://github.com/Elilif/.elemacs/core-incremental-loading.el
;; usage: (+iload '(feature1 feature2 ...))
;; then each of them will be loaded incrementally after emacs startup.

(unless (package-installed-p 'queue) (package-install 'queue))
(require 'queue)

(defvar +iload-queue (make-queue)
  "The list of features to be loaded incrementally.")
(defvar +iload-timer nil
 "The timer that runs the incremental loading process.")
(defvar +iload-delay 1.0
 "The delay between each incremental load.")

(defun +iload (features)
 "Register FEATURE to be loaded incrementally.

Each feature in FEATURES will be loaded one by one in the order."
 (dolist (feature features)
   (queue-enqueue +iload-queue feature)))

(defun +iload-do-load-one ()
 "Pop one feature from `+iload-queue' and load it.

Returns t if there are more features to load, nil otherwise."
 (when-let ((feature (queue-dequeue +iload-queue)))
   ;; (message "Incrementally loading %s..." feature)
   (let ((inhibit-message t))
     (require feature nil t))
   (not (null (queue-head +iload-queue)))))

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

;;; use-package keyword :iload
(add-to-list 'use-package-keywords :iload)

(defun use-package-normalize/:iload (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond ((symbolp arg) (list arg))
            ((listp arg) arg)
            (t (use-package-error ":iload wants a list of features"))))))

(defun use-package-handler/:iload (name-symbol keyword features rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (if (not (listp features))
        body
      (use-package-concat
       body
       `((iload ,@features))))))

(provide 'prelude-iload)
