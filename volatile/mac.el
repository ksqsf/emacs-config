(defun +set-transparent-background ()
  (interactive)
  (set-frame-parameter nil 'alpha-background 0.7))

;; Depends on a third-party patch "Blur background"
(when k|mac
  (add-to-list 'default-frame-alist '(alpha-background . 0.7))
  (+set-transparent-background))
