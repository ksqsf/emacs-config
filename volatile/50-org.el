(require 'ox-publish)

(setq prelude-site-dir (expand-file-name "~/Site"))

(setq prelude-site-style-dir (expand-file-name "~/Site/css"))
(setq prelude-site-style-output-dir (expand-file-name "~/Site/output/css"))

(setq prelude-blog-dir (expand-file-name "cicada" prelude-site-dir))
(setq prelude-blog-output-dir (expand-file-name "~/Site/output/blog"))

(setq prelude-journal-dir (expand-file-name "chaos" prelude-site-dir))
(setq prelude-journal-output-dir (expand-file-name "~/Site/output/journal"))

(setq org-publish-project-alist
      `(("site-style"
         :base-directory ,prelude-site-style-dir
         :base-extension "css"
         :publishing-directory ,prelude-site-style-output-dir
         :publishing-function org-publish-attachment
         :recursive t)
        ("blog-contents"
         :base-directory ,prelude-blog-dir
         :publishing-directory ,(expand-file-name prelude-blog-output-dir)
         :publishing-function org-html-publish-to-html
         :language "zh-CN"
         ;; Export
         :with-author nil
         :with-date t
         :with-timestamps nil
         ;; HTML
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/site.css\" />"
         :html-validation-link nil
         :html-preamble t
         :section-numbers nil
         ;; Site map
         :auto-sitemap t
         :sitemap-title "蝉"
         :sitemap-filename "index.org"
         :sitemap-style list
         :sitemap-sort-files anti-chronologically)
        ("blog-media"
         :base-directory ,prelude-blog-dir
         :base-extension "svg\\|jpg\\|gif\\|png"
         :publishing-directory ,prelude-blog-output-dir
         :publishing-function org-publish-attachment
         :recursive t)
        ("blog"
         :components ("site-style" "blog-contents" "blog-media"))))

(defun md2org ()
  "Convert the current buffer (Markdown) to Org format.

This is a best-effort convertion for Jekyll posts.  Please review
and make changes afterwards."
  (interactive)
  (replace-regexp "^---\nlayout:.*\n" "" nil (point-min) (point-max))
  (replace-regexp "^title: \"\\(.*?\\)\"" "#+title: \\1" nil (point-min) (point-max))
  (replace-regexp "^date: \\(.*?\\)$" "#+date: \\1" nil (point-min) (point-max))
  (replace-regexp "^---" "" nil (point-min) (point-max))
  (replace-regexp "<!-- *more *-->" "" nil (point-min) (point-max))
  (replace-regexp "^{% *endhighlight *%}" "#+end_src" nil (point-min) (point-max))
  (goto-char (point-min))
  (replace-regexp "{% *highlight +\\(.*?\\) *%}" "\n#+begin_src \\1" nil (point-min) (point-max))
  (setq b t)
  (while (re-search-forward "^``` ?\\(.*?\\)? *$" nil t)
    (if b
        (progn (setq b nil)
               (if (not (string= (match-string 1) ""))
                   (replace-match "#+begin_src \\1")
                 (replace-match "#+begin_src fundamental")))
      (setq b t)
      (replace-match "#+end_src")))
  (replace-regexp "^``` *$"
                  "\\,(if b (progn (setq b nil) \"#+begin_src fundamental\") (setq b t) \"#+end_src\")"
                  nil (point-min) (point-max))
  (replace-regexp "^* " "- " nil (point-min) (point-max))
  (replace-regexp "^### " "*** " nil (point-min) (point-max))
  (replace-regexp "^## " "** " nil (point-min) (point-max))
  (replace-regexp "^# " "* " nil (point-min) (point-max))
  (replace-regexp "\\[\\(.*?\\)\\](\\(.*?\\))" "[[\\2][\\1]]" nil (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "\\cc`" nil t)
    (forward-char -1)
    (insert " "))
  (goto-char (point-min))
  (while (re-search-forward "`\\cc" nil t)
    (forward-char -1)
    (insert " "))
  (replace-regexp "`" "=" nil (point-min) (point-max)))

(defun org-publish-project-alist-without-hidden ()
  (remove-if #'(lambda (project) (plist-get (cdr project) :hidden))
             org-publish-project-alist))

(define-advice org-publish (:override (project &optional force async))
  "Don't display projects marked with :hidden on the list."
  (interactive
   (list (assoc (completing-read "Publish project: "
				 (org-publish-project-alist-without-hidden) nil t)
		org-publish-project-alist)
	 current-prefix-arg))
  (let ((project (if (not (stringp project)) project
		   ;; If this function is called in batch mode,
		   ;; PROJECT is still a string here.
		   (assoc project org-publish-project-alist))))
    (cond
     ((not project))
     (async
      (org-export-async-start (lambda (_) nil)
	`(let ((org-publish-use-timestamps-flag
		,(and (not force) org-publish-use-timestamps-flag)))
	   ;; Expand components right now as external process may not
	   ;; be aware of complete `org-publish-project-alist'.
	   (org-publish-projects
	    ',(org-publish-expand-projects (list project))))))
     (t (save-window-excursion
	  (let ((org-publish-use-timestamps-flag
		 (and (not force) org-publish-use-timestamps-flag)))
	    (org-publish-projects (list project))))))))

(defun blog-post-p (&optional path)
  "Determine if PATH, or the current file if PATH is nil, is a blog post.

This is done by checking if PATH is just under
`prelude-blog-dir'."
  (file-equal-p prelude-blog-dir (or path (file-name-directory (buffer-file-name)))))

;;; If the current org file is a blog post, then images are downloaded
;;; to its own resource directory.
(add-hook 'org-mode-hook
          #'(lambda ()
              (when (blog-post-p)
                (setq org-download-image-dir
                    (file-name-sans-versions (file-name-sans-extension (buffer-file-name)))))))
