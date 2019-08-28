;;; -*- lexical-binding: t; -*-
;; Table font for perfect alignment.

;; TODO: need work on mac

;; Warning: this may not work on different font settings.
;; (eval-after-load 'org
;;   '(set-face-font 'org-table "Iosevka Term-11"))

;; (eval-after-load 'markdown
;;   '(set-face-font 'markdown-table-face "Iosevka Term-11"))

;; Even though I speak Chinese, please use English dictionary for
;; spell checking.
(setq ispell-dictionary "en_US")

;; Overwrite the builtin version, to stop `join-line' (alias of
;; `delete-indentation') insert whitespace between CJK characters.
(define-advice fixup-whitespace (:override ())
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|$\\|\\s)\\|\\cc\\|\\cj\\|\\ch")
	    (save-excursion (forward-char -1)
			    (looking-at "$\\|\\s(\\|\\s'\\|\\cc\\|\\cj\\|\\ch")))
	nil
      (insert ?\s))))

(provide 'prelude-chinese)
