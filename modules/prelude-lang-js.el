;;; -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun run-node ()
  (interactive)
  (setenv "NODE_NO_READLINE" "1")
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(provide 'prelude-lang-js)
