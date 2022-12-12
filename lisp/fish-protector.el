;;; fish-protector.el --- Protect your fish     -*- lexical-binding: t; -*-

;; THIS FILE IS A WORK IN PROGRESS.

;; 魚之護衛者 Fish Protector tries to protect your fish by monitoring
;; your use of non-focus buffers.

(defvar fp-alist nil)

(defvar fp--timer nil)
(defvar fp--timetable (make-hash-table))
(defvar fp--callbacks (make-hash-table))

;;;###autoload
(defun fp-start ()
  (interactive)
  (unless fp--timer
    (setq fp--timer (run-with-timer nil 1 'fp--tick))))

;;;###autoload
(defun fp-stop ()
  (interactive)
  (cancel-timer fp--timer)
  (setq fp--timer nil))

;;;###autoload
(defun fp-reset ()
  (interactive)
  (clrhash fp--timetable)
  (when fp--timer
    (fp-stop)
    (fp-start)))

;;;###autoload
(defun fp-show-stats ()
  (interactive)
  (with-current-buffer (get-buffer-create "*fish-protector*")
    (erase-buffer)
    (maphash (lambda (group time)
               (insert (format "%s\t%d\n" group time)))
             fp--timetable)
    (pop-to-buffer (current-buffer))))

(defun fp-alert (message)
  (let ((buf (get-buffer-create "*fish-protector-alert*")))
    (with-current-buffer buf
      (insert "-----\n")
      (insert message)
      (insert "\n-----\n\n")
      (display-buffer (current-buffer))
      (goto-char (point-max)))))

(defun fp-add-reach-limit (group limit-secs callback)
  (cl-assert (and (symbolp group)
                  (integerp limit-secs)))
  (puthash group (cons limit-secs callback) fp--callbacks))

(defun fp-remove-limit-callback (group)
  (remhash group fp--callbacks))

(defun fp--tick ()
  (let ((buf-group (catch 'found
                       (dolist (rule fp-alist)
                         (when (fp--group-rule-match-p (cdr rule) (current-buffer))
                           (throw 'found (car rule)))))))
    (when buf-group
      (let* ((old (gethash buf-group fp--timetable 0))
             (new (1+ old)))
        (puthash buf-group new fp--timetable)
        (let* ((limit/cb (gethash buf-group fp--callbacks (cons most-positive-fixnum 'ignore)))
               (limit (car limit/cb))
               (cb (cdr limit/cb)))
          (when (> new limit)
            (funcall cb new)))))))

(defun fp--group-rule-match-p (rule &optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (pcase-exhaustive rule
    ;; Always match
    ('t t)
    ('nil nil)

    ;; (and rules...)
    ((and (pred listp)
          (app car 'and))
     (catch 'foo
       (let ((rules (cdr rule)))
         (dolist (rule rules)
           (unless (fp--group-rule-match-p rule buffer)
             (throw 'foo nil))))
       t))

    ;; (or rules...)
    ((and (pred listp)
          (app car 'or))
     (catch 'foo
       (let ((rules (cdr rule)))
         (dolist (rule rules)
           (when (fp--group-rule-match-p rule buffer)
             (throw 'foo t))))
       nil))

    ;; Major modes
    ((or `(mode ,mode)
         `(major ,mode)
         `(major-mode ,mode))
     (derived-mode-p mode))

    ;; Minor modes
    ((or `(minor ,mode)
         `(minor-mode ,mode))
     (and (boundp mode)
          (eval mode)))

    ;; File names
    ((pred stringp)
     (and buffer-file-name
          (string-match rule buffer-file-name)))
    ((or `(file ,regexp)
         `(path ,regexp))
     (and buffer-file-name
          (string-match regexp buffer-file-name)))

    ;; Projects
    (`(project ,regexp)
     (and buffer-file-name
          (let* ((proj (project-current))
                 (root (and proj (project-root proj))))
            (if root
                (string-match regexp root)
              nil))))

    ;; Arbitrary predicates
    (`(pred ,p)
     (funcall p buffer))))

;;;
;;; FIXME: To be removed
;;;
(setq fp-alist
      '((:telega . (or (mode telega-root-mode)
                       (mode telega-chat-mode)))
        (:emacs . (project "\\.emacs\\.d"))))

(fp--group-rule-match-p '(:telega . (or (mode telega-root-mode)
                       (mode telega-chat-mode))))

(fp-add-reach-limit :telega
                    (* 30 60)
                    (lambda (cur-sec)
                      (if (> cur-sec (* 60 60))
                          (progn
                            (kill-buffer "*Telega Root*")
                            (fp-alert "Hard limit of Telega reached!"))
                        (fp-alert "Soft limit of Telega reached! Please go away from Telega!!"))))

(provide 'fish-protector)
;;; fish-protector.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("fp-" . "fish-protector-"))
;; End:
