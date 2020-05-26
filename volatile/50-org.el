(require 'ox-publish)

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

(setq k/date-format "%Y 年 %m 月 %d 日")

;;; My own wrapping around org-publish


(defclass k/project! ()
  ((name :initarg :name
         :type string)
   (url :initarg :url
        :type string)
   (css :initarg :css
        :type string
        :initform "../css/site.css")
   (input-dir :initarg :input-dir
              :type string)
   (output-dir :initarg :output-dir
               :type string)
   (language :initarg :language
             :type string
             :initform "zh-CN")
   (preamble :initarg :preamble
             :initform "")
   (postamble :initarg :postamble
              :initform "<footer></footer>"))
  :abstract t)

(cl-defgeneric k/static-config ((pj k/project!))
  (list (format "k-%s-static" (oref pj :name))
        :hidden t
        :base-directory (oref pj :input-dir)
        :base-extension ".*"
        :exclude ".*\\.org$"
        :publishing-function #'org-publish-attachment
        :publishing-directory (oref pj :output-dir)
        :recursive t))

(cl-defgeneric k/pages-config ((pj k/project!))
  (list (format "k-%s-pages" (oref pj :name))
        :hidden t
        :base-directory (oref pj :input-dir)
        :base-extension "org"
        :exclude (regexp-opt '("feed.org"))
        :publishing-directory (oref pj :output-dir)
        :publishing-function #'org-html-publish-to-html
        :recursive t
        :language (oref pj :language)
        :html-head (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />"
                           (oref pj :css))
        :htmlized-source nil
        :html-doctype "html5"
        :html-checkbox-type 'unicode
        :html-html5-fancy t
        :html-postamble (oref pj :postamble)
        :html-preamble (oref pj :preamble)))

(cl-defmethod k/configs ((pj k/project!))
  (list (k/static-config pj)
        (k/pages-config pj)
        (let ((name (oref pj :name)))
          (list (format "k-%s" name)
                :components (list (format "k-%s-static" name)
                                  (format "k-%s-pages" name))))))


(defclass k/blog! (k/project!)
  ((rss? :initarg :rss?
         :type boolean
         :initform t)
   (archive? :initarg :archive?
             :type boolean
             :initform t)))

(cl-defmethod k/rss-config ((pj k/blog!))
  (list (format "k-%s-rss" (oref pj :name))
        :hidden t
        :base-directory (oref pj :input-dir)
        :base-extension "org"
        :recursive t
        :exclude (regexp-opt '("index.org" "404.org" "feed.org"))
        :publishing-function 'k/blog-rss-publish
        :publishing-directory (oref pj :output-dir)
        :language (oref pj :language)
        :html-link-home (oref pj :url)
        :rss-link-home (oref pj :url)
        :html-link-use-abs-url t
        :auto-sitemap t
        :sitemap-filename "feed.org"
        :sitemap-title (oref pj :name)
        :sitemap-style 'list
        :sitemap-sort-files 'anti-chronologically
        :sitemap-function 'k/blog-format-rss
        :sitemap-format-entry 'k/blog-format-rss-entry))

(defun k/blog-rss-publish (plist filename pub-dir)
  "Publish RSS with PLIST, only when FILENAME is 'feed.org'.

PUB-DIR is when the output will be placed."
  (if (string= "feed.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

(defun k/blog-format-rss (title list)
  "Generate RSS feed, as a string.

TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
          (org-list-to-subtree list 0 '(:icount "" :istart ""))))

(defun k/blog-format-rss-entry (entry style project)
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
             (org-set-property "RSS_TITLE" title)
             (org-set-property "RSS_PERMALINK" link)
             (org-set-property "PUBDATE" date)
             ;; (insert-file-contents file) ;; BUG??
             (buffer-string))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(cl-defmethod k/pages-config ((pj k/blog!))
  (let ((base-config (cl-call-next-method pj)))
    (when (oref pj :archive?)
      (setq base-config
            (append base-config
                    (list :auto-sitemap t
                          :sitemap-filename "index.org"
                          :sitemap-title (oref pj :name)
                          :sitemap-style 'list
                          :sitemap-sort-files 'anti-chronologically
                          :sitemap-function 'k/blog-format-archive
                          :sitemap-format-entry 'k/blog-format-archive-entry))))
    base-config))

(defun k/blog-format-archive (title list)
  "Default site map, as a string.
TITLE is the the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n#+ATTR_HTML: :class blog-posts\n"
	  (org-list-to-org list)))

(defun k/blog-format-archive-entry (entry style project)
  "ENTRY is an Org page."
  (message "GEN ENTRY: %s" entry)
  (cond ((not (directory-name-p entry))
         (format "*[[file:%s][%s]]*
                 #+HTML: <p class='pubdate'>%s</p>"
                 entry
                 (org-publish-find-title entry project)
                 (format-time-string k/date-format
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(cl-defmethod k/configs ((pj k/blog!))
  (let ((base-configs (cl-call-next-method pj)))
    (if (oref pj :rss?)
        (progn
          (push (k/rss-config pj) base-configs)
          (let* ((name (oref pj :name))
                 (components (caddr (assoc (format "k-%s" name) base-configs))))
            (push (format "k-%s-rss" name) components)
            (setf (caddr (assoc (format "k-%s" name) base-configs))
                  components))
          base-configs)
      base-configs)))


(defclass k/send! (k/project!)
  ((extension :initarg :extension
              :initform ".*"
              :type string))
  :documentation "Send the files from INPUT-DIR to OUTPUT-DIR as-is.")

(cl-defmethod k/configs ((pj k/send!))
  (list (list (format "%s" (oref pj :name))
              :base-directory (oref pj :input-dir)
              :base-extension (oref pj :extension)
              :publishing-directory (oref pj :output-dir)
              :publishing-function #'org-publish-attachment)))


(defclass k/wiki! (k/project!) ()
  :documentation "Wiki project type.")

(cl-defmethod k/pages-config ((pj k/wiki!))
  (let ((base-config (cl-call-next-method pj)))
    (cons (car base-config)
          (append (list :auto-sitemap t
                        :sitemap-title (oref pj :name)
                        :sitemap-filename "index.org"
                        :sitemap-style 'tree)
                  (cdr base-config)))))


(defclass k/page! (k/project!) ()
  :documentation "A single page (a directory containing index.org and other static files)")


(defclass k/group! (k/project!)
  ((components :initarg :components)))

(cl-defmethod k/configs ((pj k/group!))
  (list (list (format "%s" (oref pj :name))
              :components (oref pj :components))))



(setq org-publish-project-alist nil)    ; FIXME: to be removed
(defvar k/projects nil)

(defun k/setup! ()
  (dolist (pj k/projects)
    (setq org-publish-project-alist
          (append org-publish-project-alist (k/configs pj)))))


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
       <a%s href=\"/about\"> about</a>
     </div>
     <div class=\"clearfix\"></div>
   </header>"
   (if (eq active 'blog) " class=\"active\"" "")
   (if (eq active 'journal) " class=\"active\"" "")
   (if (eq active 'koishi) " class=\"active\"" "")
   (if (eq active 'wiki) " class=\"active\"" "")
   (if (eq active 'about) " class=\"active\"" "")))

(setq input-dir (expand-file-name "~/Site"))
(setq output-dir (expand-file-name "~/Site/ksqsf.moe-deploy"))

(setq k/projects
      (list (k/send! :name "css"
                     :input-dir (concat input-dir "/css")
                     :output-dir (concat output-dir "/css"))
            (k/send! :name "homepage"
                     :input-dir (concat input-dir "/homepage")
                     :output-dir "/tmp/hhh")
            (k/blog! :name "Cicada"
                     :url "https://ksqsf.moe/blog"
                     :input-dir (concat input-dir "/cicada")
                     :output-dir (concat output-dir "/blog")
                     :preamble #'(lambda (entry) (k/site-nav 'blog)))
            (k/blog! :name "Chaos"
                     :url "https://ksqsf.moe/journal"
                     :input-dir (concat input-dir "/chaos")
                     :output-dir (concat output-dir "/journal")
                     :preamble #'(lambda (entry) (k/site-nav 'journal)))
            (k/page! :name "about"
                     :url "https://ksqsf.moe/about"
                     :input-dir (concat input-dir "/about")
                     :output-dir (concat output-dir "/about")
                     :preamble #'(lambda (entry) (k/site-nav 'about)))
            (k/page! :name "koishi"
                     :url "https://ksqsf.moe/koishi"
                     :input-dir (concat input-dir "/koishi")
                     :output-dir (concat output-dir "/koishi")
                     :preamble #'(lambda (entry) (k/site-nav 'koishi)))
            (k/wiki! :name "wiki"
                     :input-dir "~/org/wiki" ; NOTE
                     :output-dir (concat output-dir "/wiki")
                     :preamble #'(lambda (entry) (k/site-nav 'wiki)))
            (k/group! :name "ksqsf.moe"
                      :components '("k-Cicada" "k-wiki" "k-Chaos" "k-koishi" "css" "homepage" "k-about"))))

(k/setup!)
