(setq test nil)

(when test

  (defun k|smart-input-enable ()
    (interactive)
    (add-hook 'meow-normal-mode-hook #'k|meow-input-dwim)
    (add-hook 'meow-insert-mode-hook #'k|meow-input-dwim))

  (defun k|smart-input-disable ()
    (interactive)
    (remove-hook 'meow-normal-mode-hook #'k|meow-input-dwim)
    (remove-hook 'meow-insert-mode-hook #'k|meow-input-dwim))

  (defvar k|input-sources (cons "com.apple.keylayout.ABC" "com.apple.inputmethod.SCIM.Shuangpin"))

  (defun k|input-primary ()
    (car k|input-sources))

  (defun k|input-secondary ()
    (cdr k|input-sources))

  (defun k|input-select (kind)
    (pcase kind
      ('primary (k|macism (k|input-primary)))
      ('secondary (k|macism (k|input-secondary)))
      (_ (error "Unknown kind of input sources"))))

  (defun k|macism (source)
    (process-lines "macism" source))

  (defun k|meow-input-dwim ()
    (interactive)
    (if meow-insert-mode
        (k|input-select 'secondary)
      (k|input-select 'primary))))
