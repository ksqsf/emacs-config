(add-to-list 'load-path (expand-file-name "~/src/pest-mode"))
(autoload 'pest-mode "pest-mode")
(add-to-list 'auto-mode-alist '("\\.pest\\'" . pest-mode))
