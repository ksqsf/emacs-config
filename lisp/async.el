;; -*- lexical-binding: t; -*-

;; Experiment on low-latency emacs.  async.el provides the fundamental
;; abstractions, using the familiar `async' and `await' duo.  The
;; `async' macro marks a region of code whose evaluation may run
;; concurrently, and the result is stored in a promise object.  When
;; you need the result, you can use `await'.
;;
;; However, different from CPS-transformation-based stackless
;; coroutines, the approach envisioned here is a stackful one,
;; a.k.a. green threads or fibers.  The async code block is executed
;; immediately, and everything remains the same: you don't need to
;; change every function definition.
;;
;; For example, this is totally ok:
;;
;;    (defun foo () (bar))
;;    (defun bar () (blocking))
;;    (setq promise (async (foo)))
;;    ; other computation
;;    (use (await promise))
;;
;; You don't need to use something like `async-defun' for `foo' or
;; `bar'.
;;
;; The stackful abstration provides a smooth path for migration: you
;; can migrate piece by piece.
;;
;; Currently, it is implemented atop emacs's system threading support,
;; which is quite heavyweight.  But it should be almost semantically
;; equivalent.

(defun promise-create (executor)
  "Create a promise that runs EXECUTOR in a thread.
EXECUTOR is called with (resolve reject) functions."
  (let* ((state 'pending)
         (result nil)
         (error-value nil)
         (mutex (make-mutex "promise-mutex"))
         (promise (vector 'promise mutex state result error-value)))

    ;; Create thread to run executor
    (make-thread
     (lambda ()
       (condition-case err
           (funcall executor
                    ;; resolve function
                    (lambda (value)
                      (with-mutex mutex
                        (when (eq (aref promise 2) 'pending)
                          (aset promise 2 'resolved)
                          (aset promise 3 value))))
                    ;; reject function
                    (lambda (reason)
                      (with-mutex mutex
                        (when (eq (aref promise 2) 'pending)
                          (aset promise 2 'rejected)
                          (aset promise 4 reason)))))
         (error
          (with-mutex mutex
            (when (eq (aref promise 2) 'pending)
              (aset promise 2 'rejected)
              (aset promise 4 err))))))
     "promise-executor")
    
    promise))

(defun promise--mutex (promise) (aref promise 1))
(defun promise--state (promise) (aref promise 2))
(defun promise--result (promise) (aref promise 3))
(defun promise--error (promise) (aref promise 4))

(defun promise-p (obj)
  "Return t if OBJ is a promise."
  (and (vectorp obj)
       (> (length obj) 0)
       (eq (aref obj 0) 'promise)))

(defun await (promise)
  "Wait for PROMISE to resolve and return its value.
If PROMISE is rejected, signal error.
If not a promise, return value as-is."
  (cond
   ;; Not a promise - return as-is
   ((not (promise-p promise))
    (error "Only promises can be awaited"))

   ;; Wait for promise to complete using polling
   (t
    (let ((mutex (promise--mutex promise)))
      ;; Poll until the promise is no longer pending
      (while (with-mutex mutex
               (eq (promise--state promise) 'pending))
        (sleep-for 0.01))  ; Sleep briefly to avoid busy-waiting

      ;; Return result or signal error
      (with-mutex mutex
        (if (eq (promise--state promise) 'resolved)
            (promise--result promise)
          (let ((err (promise--error promise)))
            ;; condition-case returns (error-symbol . data)
            ;; We need to ensure data is a proper list for signal
            (if (consp err)
                (let ((err-symbol (car err))
                      (err-data (cdr err)))
                  (signal err-symbol 
                          (if (listp err-data) err-data (list err-data))))
              (error "Promise rejected: %S" err)))))))))

(defun async-call (fn &rest args)
  "Call FN with ARGS asynchronously and return a promise."
  (promise-create
   (lambda (resolve reject)
     (condition-case err
         (let ((result (apply fn args)))
           (funcall resolve result))
       (error (funcall reject err))))))

(defmacro async (&rest body)
  "Execute BODY asynchronously and return a promise.
Within BODY, you can use (await PROMISE) to wait for promises."
  `(promise-create
    (lambda (resolve reject)
      (condition-case err
          (funcall resolve (progn ,@body))
        (error (funcall reject err))))))

(defun mapcar-concurrently (fn list)
  "Map FN over LIST asynchronously in parallel.
Returns a list of results in the same order."
  (let ((promises (mapcar (lambda (item)
                           (async-call fn item))
                         list)))
    (mapcar #'await promises)))

(provide 'async)
