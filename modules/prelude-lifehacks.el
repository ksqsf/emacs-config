;;; prelude-lifehacks.el ---                         -*- lexical-binding: t; -*-

(defun web-search ()
  "Search the Internet."
  (interactive)
  (let* ((input (read-from-minibuffer "Search: "))
         (kagi-url (format "https://kagi.com/search?q=%s" (url-encode-url input))))
    (browse-url kagi-url)))

(defalias 'kagi 'web-search)

(provide 'prelude-lifehacks)
;;; prelude-lifehacks.el ends here
