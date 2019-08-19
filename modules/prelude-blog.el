;;; -*- lexical-binding: t; -*-
(defcustom blog-posts-dir (expand-file-name "~/Site/jekyll/_posts/")
  "The directory for your post Markdown files."
  :group 'prelude
  :type '(string))

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

(defun find-blog-post ()
  "Find one of your blog posts."
  (interactive)
  (find-file (completing-read "Post: " (directory-files blog-posts-dir nil "^[^\\.].*"))))

(provide 'prelude-blog)
