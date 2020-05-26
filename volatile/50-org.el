(require 'ox-publish)

(setq prelude-site-dir (expand-file-name "~/Site"))

(setq prelude-site-style-dir (expand-file-name "~/Site/css"))
(setq prelude-site-style-output-dir (expand-file-name "~/Site/output/css"))

(setq prelude-blog-dir (expand-file-name "cicada" prelude-site-dir))
(setq prelude-blog-output-dir (expand-file-name "~/Site/output/blog"))

(setq prelude-journal-dir (expand-file-name "chaos" prelude-site-dir))
(setq prelude-journal-output-dir (expand-file-name "~/Site/output/journal"))

(setq prelude-wiki-dir (expand-file-name "~/org/wiki"))
(setq prelude-wiki-output-dir (expand-file-name "~/Site/output/wiki"))

(setq prelude-site-homepage-dir (expand-file-name "~/Site/homepage"))
(setq prelude-site-homepage-output-dir (expand-file-name "~/Site/output"))

(setq prelude-pages-dir (expand-file-name "~/Site/pages"))
(setq prelude-pages-output-dir (expand-file-name "~/Site/output"))

(setq org-publish-project-alist
      `(("site-homepage"
         :base-directory ,prelude-site-homepage-dir
         :base-extension "html\\|jpg\\|png\\|css"
         :publishing-directory ,prelude-site-homepage-output-dir
         :publishing-function org-publish-attachment
         :recursive t)
        ("site-style"
         :base-directory ,prelude-site-style-dir
         :base-extension "css"
         :publishing-directory ,prelude-site-style-output-dir
         :publishing-function org-publish-attachment
         :recursive t)
        ("pages"
         :base-directory ,prelude-pages-dir
         :base-extension "org"
         :publishing-directory ,prelude-pages-output-dir
         :publishing-function org-html-publish-to-html
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/site.css\" />"
         :html-validation-link nil
         :html-preamble k/about-preamble)
        ("blog-posts"
         :base-directory ,prelude-blog-dir
         :publishing-function org-html-publish-to-html
         :publishing-directory ,(expand-file-name prelude-blog-output-dir)
         :exclude ,(regexp-opt '("feed.org" "index.org"))
         :language "zh-CN"
         ;; Export
         :section-numbers nil
         :with-author nil
         :with-date t
         :with-timestamps nil
         ;; HTML
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/site.css\" />"
         :html-validation-link nil
         :html-preamble k/blog-post-preamble
         ;; Site map
         :auto-sitemap t
         :sitemap-title "蝉"
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically
         :sitemap-function k/sitemap
         :sitemap-format-entry k/sitemap-format-entry)
        ("blog-media"
         :base-directory ,prelude-blog-dir
         :base-extension "svg\\|jpg\\|gif\\|png"
         :publishing-directory ,prelude-blog-output-dir
         :publishing-function org-publish-attachment
         :recursive t)
        ("blog-rss"
         :base-directory ,prelude-blog-dir
         :base-extension "org"
         :recursive nil
         :exclude ,(regexp-opt '("index.org" "404.org" "feed.org"))
         :publishing-function k/publish-rss
         :publishing-directory ,prelude-blog-output-dir
         :rss-extension "xml"
         :html-link-home "https://ksqsf.moe/blog"
         :rss-link-home "https://ksqsf.moe/blog"
         :rss-image-url nil
         :html-link-use-abs-url t
         :language "zh-CN"
         :section-number nil
         :table-of-contents nil
         :auto-sitemap t
         :sitemap-filename "feed.org"
         :sitemap-title "蝉"
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :sitemap-function k/format-rss-feed
         :sitemap-format-entry k/format-rss-feed-entry)
        ("blog"
         :components ("site-style" "blog-rss" "blog-posts" "blog-media"))
        ("journal-posts"
         :base-directory ,prelude-journal-dir
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :publishing-directory ,prelude-journal-output-dir
         :recursive t
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/site.css\" />"
         :html-preamble k/journal-post-preamble
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "混乱"
         :sitemap-style list
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry k/sitemap-format-entry)
        ("wiki-pages"
         :base-directory ,prelude-wiki-dir
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :publishing-directory ,prelude-wiki-output-dir
         :recursive t
         :language "zh-CN"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/site.css\" />"
         :html-validation-link nil
         :html-preamble k/wiki-page-preamble
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Wiki")))

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
  (if (null (or (buffer-file-name) path))
      nil
    (let ((path (or path (file-name-directory (buffer-file-name)))))
      (and path
           (file-equal-p prelude-blog-dir path)))))

;;; If the current org file is a blog post, then images are downloaded
;;; to its own resource directory.
(add-hook 'org-mode-hook
          #'(lambda ()
              (when (blog-post-p)
                (setq org-download-image-dir
                      (file-name-sans-versions (file-name-sans-extension (buffer-file-name)))))))

(setq k/date-format "%Y 年 %m 月 %d 日")

(defun k/sitemap (title list)
  "Default site map, as a string.
TITLE is the the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n#+ATTR_HTML: :class blog-posts\n"
	  (org-list-to-org list)))

(defun k/sitemap-format-entry (entry style project)
  "ENTRY is an Org page."
  (cond ((not (directory-name-p entry))
         (format "*[[file:%s][%s]]*
                 #+HTML: <p class='pubdate'>%s</p>"
                 entry
                 (org-publish-find-title entry project)
                 ;; (or (k/post-hitogoto entry project) "")
                 (format-time-string k/date-format
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

;; (defun k/post-hitogoto (file project)
;;   "Find the title of FILE in PROJECT."
;;   (let ((file (org-publish--expand-file-name file project)))
;;     (or (org-publish-cache-get-file-property file :hitogoto nil t)
;; 	(let* ((parsed (org-publish-find-property file :hitogoto project))
;; 	       (hitogoto
;; 		(if parsed
;; 		    (org-no-properties
;; 		     (org-element-interpret-data parsed))
;; 		  (file-name-nondirectory (file-name-sans-extension file)))))
;; 	  (org-publish-cache-set-file-property file :hitogoto hitogoto)
;; 	  hitogoto))))

(defun k/site-nav (active)
  "Generate a navbar."
  (format
   "<header>
     <div class=\"site-title\">
       <a href=\"/\">
         <img src=\"\">
       </a>
     </div>
     <div class=\"site-nav\">
       <a%s href=\"/blog\"> blog</a>
       <a%s href=\"/journal\"> journal</a>
       <a%s href=\"/koishi\"> koishi</a>
       <a%s href=\"/wiki\"> wiki</a>
       <a%s href=\"/about.html\"> about<a/>
     </div>
     <div class=\"clearfix\"></div>
   </header>"
   (if (eq active 'blog) " class=\"active\"" "")
   (if (eq active 'journal) " class=\"active\"" "")
   (if (eq active 'coishi) " class=\"active\"" "")
   (if (eq active 'wiki) " class=\"active\"" "")
   (if (eq active 'about) " class=\"active\"" "")))

(defun k/blog-post-preamble (entry)
  "ENTRY is a plist."
  (k/site-nav 'blog))

(defun k/wiki-page-preamble (entry)
  (k/site-nav 'wiki))

(defun k/journal-post-preamble (entry)
  (k/site-nav 'journal))

(defun k/about-preamble (entry)
  (k/site-nav 'about))

(defun k/publish-rss (plist filename pub-dir)
  "Publish RSS with PLIST, only when FILENAME is 'feed.org'.
PUB-DIR is when the output will be placed."
  (if (equal "feed.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

(defun k/format-rss-feed (title list)
  "Generate RSS feed, as a string.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
          (org-list-to-subtree list '(:icount "" :istart ""))))

(defun k/format-rss-feed-entry (entry style project)
  "Format ENTRY for the RSS feed.
ENTRY is a file name.  STYLE is either 'list' or 'tree'.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
         (let* ((file (org-publish--expand-file-name entry project))
                (title (org-publish-find-title entry project))
                (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
                (link (concat (file-name-sans-extension entry) ".html")))
           (with-temp-buffer
             (insert (format "* [[file:%s][%s]]\n" file title))
             (org-set-property "RSS_PERMALINK" link)
             (org-set-property "PUBDATE" date)
             ;; (insert-file-contents file) ;; BUG??
             (buffer-string))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))
