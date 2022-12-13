;;; NOTE: PLEASE DO NOT USE THIS FILE AND DON'T BELIEVE ANY WORDS IN
;;; IT.  This file does not contain any usable code.

;;; fluidsynth.el --- An interface to fluidsynth     -*- lexical-binding: t; -*-

(defun make-fluidsynth (soundfont-path)
  (let ((proc (make-process :name "fluidsynth"
                            :buffer " *fluidsynth*"
                            :command `("fluidsynth" ,soundfont-path))))
    (make-record )))

(defun fluidsynth-proc (fs)
  (aref fs 0))

(defun fluidsynth-sheet (fs)
  (aref fs 1))

(defun fluidsynth-shee)

(provide 'fluidsynth)
;;; fluidsynth.el ends here
