;;; prelude-lifehacks.el ---                         -*- lexical-binding: t; -*-
;;;
;;; A collection of lifehacks.

(defvar web-search-history nil)

(defun web-search ()
  "Search the Internet."
  (interactive)
  (let* ((input (read-from-minibuffer "Search: " nil nil nil 'web-search-history))
         (kagi-url (format "https://kagi.com/search?q=%s" (url-encode-url input))))
    (browse-url kagi-url)))

(defalias 'kagi 'web-search)

(use-package life-calendar
  :vc (:fetcher github :repo "vshender/emacs-life-calendar")
  :ensure t)

(provide 'prelude-lifehacks)
;;; prelude-lifehacks.el ends here
