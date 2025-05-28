;; -*- lexical-binding: t; -*-

(defun frame-appear-animation (&optional frame from to inc)
  (interactive)
  (setq from (or from 0))
  (setq to (or to 100))
  (setq inc (or inc 1))
  (dolist (i (number-sequence from to inc))
    (let ((alpha (* i 0.01)))
      (set-frame-parameter frame 'alpha alpha)
      (sit-for 0.001))))

(defun frame-disappear-animation (&optional frame from to dec)
  (setq dec (or dec 1))
  (frame-appear-animation frame from to (- 0 dec)))

;; (progn (frame-disappear-animation nil 97 10) (frame-appear-animation nil 10 97))
