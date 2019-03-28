(defcustom blog-posts-dir (expand-file-name "~/Site/jekyll/_posts/")
  "The directory for your post Markdown files.")

(defun blog--header (layout title time)
  (let ((datetime (format-time-string "%Y-%m-%d %H:%M:%S" time)))
    (format "---
layout: %s
title: \"%s\"
date: %s
---
" layout (replace-regexp-in-string "\"" "\\\\\"" title) datetime)))

(defun blog-new-post (title permalink)
  (interactive "sTitle: \nsPermalink for post '%s': \n")
  (let* ((time (current-time))
	 (date (format-time-string "%Y-%m-%d" time))
	 (filename (format "%s-%s.md" date permalink))
	 (header (blog--header "post" title time)))
    (find-file (expand-file-name filename blog-posts-dir))
    (insert header)
    (newline)))

(provide 'prelude-blog)
