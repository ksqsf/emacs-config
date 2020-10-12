;;; EMACScript!

(with-eval-after-load 'eww
  (defun EMACScript (script)
    (when (equal (dom-attr script 'type) "text/emacscript")
      (eval (read (dom-text script)) 'lexical)))
  (add-to-list 'shr-external-rendering-functions
               '(script . EMACScript)))
