;;; prelude-lifehacks.el ---                         -*- lexical-binding: t; -*-
;;;
;;; A collection of lifehacks.

(defvar web-search-history nil)

(defun web-search (url)
  "Search the Internet."
  (interactive)
  (let* ((input (read-from-minibuffer "Search: " nil nil nil 'web-search-history)))
    (browse-url (concat url (url-encode-url input)))))

(defun kagi ()
  "Search with Kagi."
  (interactive)
  (web-serach "https://kagi.com/search?q="))

(defun ddg ()
  "Search with DuckDuckGo."
  (interactive)
  (web-search "https://duckduckgo.com/?q="))

(use-package life-calendar
  :vc (:fetcher github :repo "vshender/emacs-life-calendar")
  :ensure t)

(provide 'prelude-lifehacks)
;;; prelude-lifehacks.el ends here
