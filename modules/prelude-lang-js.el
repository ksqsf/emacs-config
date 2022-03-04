;;; -*- lexical-binding: t; -*-

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :init
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(defun run-node ()
  (interactive)
  (setenv "NODE_NO_READLINE" "1")
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(provide 'prelude-lang-js)
