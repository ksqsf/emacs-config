;; Table font for perfect alignment.
;; Warning: this may not work on different font settings.
(eval-after-load 'org
  '(set-face-font 'org-table "Iosevka Term-11"))

(eval-after-load 'markdown
  '(set-face-font 'markdown-table-face "Iosevka Term-11"))

;; Even though I speak Chinese, please use English dictionary for
;; spell checking.
(setq ispell-dictionary "en_US")

(provide 'prelude-chinese)
