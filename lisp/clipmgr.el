(require 'ring)
(require 'ivy)

(defvar clip-ring-size 100)
(defvar clip-ring (make-ring clip-ring-size))
(defvar clip-timer nil)

;;;###autoload
(defun clipmgr-start ()
  (interactive)
  (setq clip-timer
        (run-with-timer nil 1 'clipmgr--maybe-insert)))

;;;###autoload
(defun clipmgr-stop ()
  (interactive)
  (when clip-timer
    (cancel-timer clip-timer)))

;;;###autoload
(defun clipmgr-select ()
  (interactive)
  (when (ring-empty-p clip-ring)
    (error "The clip ring is empty."))
  (ivy-read "Select a clip to restore: "
            (cl-loop for it in (ring-elements clip-ring)
                     collect (cons (cdr (assoc 'STRING it)) it))
            :action (lambda (selection)
                      (let ((clip (cdr selection)))
                        (clipmgr--restore clip)))))

(defun clipmgr--current-clip ()
  (let ((targets (gui-get-selection 'CLIPBOARD 'TARGETS)))
    (if (not (vectorp targets))
        nil
      (cl-loop for it in (seq-into targets 'list)
               collect (cons it (gui-get-selection 'CLIPBOARD it))))))

(defun clipmgr--maybe-insert ()
  (let ((cur-clip (clipmgr--current-clip)))
    (when cur-clip
      (if (ring-empty-p clip-ring)
          (ring-insert clip-ring cur-clip)
        (if (equal (ring-ref clip-ring 0) cur-clip)
            nil
          (ring-insert clip-ring cur-clip))))))

;; TODO: This does not work!
(defun clipmgr--restore (clip)
  (let ((result (cdr (assoc 'STRING clip))))
    (dolist (pair clip)
      (let ((key (car pair))
            (value (cdr pair)))
        (unless (eq key 'STRING)
          (setq result (propertize result key value)))))
    (gui-set-selection 'CLIPBOARD result)
    (gui-set-selection 'PRIMARY result)))

(provide 'clipmgr)
