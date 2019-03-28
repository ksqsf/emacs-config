;;; Config for web dev, including (node)js (wrong?)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun run-node ()
  (interactive)
  (setenv "NODE_NO_READLINE" "1")
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(provide 'prelude-web)
