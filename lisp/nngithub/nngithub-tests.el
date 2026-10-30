;;; nngithub-tests.el --- Tests for nngithub -*- lexical-binding: t; -*-
;; Assisted-By: Codex:gpt-6-astra
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Run: emacs -Q --batch -L lisp/nngithub -l nngithub-tests \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'nngithub)

(defmacro nngithub-test--isolated (&rest body)
  "Run BODY with an isolated server and disposable disk cache."
  (declare (indent 0))
  `(let ((nngithub-directory (make-temp-file "nngithub-test-" t))
         (nnoo-state-alist (copy-tree nnoo-state-alist))
         (nngithub--state nil) (nngithub--loaded nil)
         (nngithub-host "github.com")
         (gnus-secondary-select-methods nil)
         (gnus-server-alist nil)
         (nntp-server-buffer (generate-new-buffer " *nngithub-test*")))
     (unwind-protect
         (progn
           (nngithub-open-server "test")
           (nngithub--load)
           ,@body)
       (kill-buffer nntp-server-buffer)
       (delete-directory nngithub-directory t))))

(defun nngithub-test--node (id &optional date)
  "Fixture node with ID and optional DATE."
  (copy-tree
   `((id . ,id) (body . "Hello 世界\n**Markdown**")
     (createdAt . ,(or date "2026-01-01T00:00:00Z"))
     (updatedAt . "2026-01-02T00:00:00Z")
     (url . "https://github.com/a/b/issues/1") (author . ((login . "alice")))
     (number . 1) (title . "A title") (state . "OPEN"))))

(defun nngithub-test--seed (group)
  "Seed GROUP with a root and a reply, returning the reply."
  (let* ((root (nngithub-test--node "ROOT"))
         (a (nngithub--article root root group))
         (b (nngithub--article (nngithub-test--node "COMMENT") root group "ROOT"))
         (state (nngithub--group group)))
    (setf (plist-get a :number) 1 (plist-get b :number) 2
          (plist-get state :articles) (list a b)
          (plist-get state :next) 3 (plist-get state :synced) t)
    b))

(defmacro nngithub-test--message (headers &rest body)
  "Create a Message buffer with HEADERS, then run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (message-mode)
     (insert ,headers "\n" mail-header-separator "\nReply 世界\n")
     ,@body))

(ert-deftest nngithub-group-validation ()
  (should (equal (nngithub--parse-group "owner/repo.name.pulls")
                 '("owner" "repo.name" "pulls")))
  (dolist (name '("a/b" "../b.issues" "a/b.issues\nCc: victim" "a/b.issues,a/c.issues"))
    (should-error (nngithub--parse-group name))))

(ert-deftest nngithub-server-list-does-not-import-history ()
  (nngithub-test--isolated
   (cl-letf (((symbol-function 'nngithub--connection)
              (lambda (prefix field _args _fields)
                (should (equal prefix "target: viewer"))
                (should (equal field "watching"))
                '(((nameWithOwner . "a/b") (hasIssuesEnabled . t))
                  ((nameWithOwner . "c/d") (hasDiscussionsEnabled . t)))))
             ((symbol-function 'nngithub--sync)
              (lambda (&rest _) (ert-fail "Browsing fetched history"))))
     (should (nngithub-request-list "test"))
     (should-not (plist-get nngithub--state :groups))
     (should (equal (sort (nngithub--groups) #'string<)
                    '("a/b.issues" "a/b.pulls" "c/d.discussions" "c/d.pulls")))
     (with-current-buffer nntp-server-buffer
       (should (string-match-p "a/b.issues 0 1 y" (buffer-string)))))))

(ert-deftest nngithub-standard-group-creation-validates-repository ()
  (nngithub-test--isolated
   (cl-letf (((symbol-function 'nngithub--api)
              (lambda (_query variables)
                (should (equal variables '((owner . "a") (name . "b"))))
                '((repository . ((id . "REPO") (hasIssuesEnabled . t)))))))
     (should (nngithub-request-create-group "a/b.issues" "test"))
     (should-not (nngithub-request-create-group "a/b.discussions" "test"))
     (should-not (assoc "a/b.discussions" (plist-get nngithub--state :groups)))
     (should (assoc "a/b.issues" (plist-get nngithub--state :groups))))))

(ert-deftest nngithub-retrieve-groups-does-not-discover-or-import ()
  (nngithub-test--isolated
   (cl-letf (((symbol-function 'nngithub--api)
              (lambda (&rest _) (ert-fail "Unexpected API request"))))
     (should (eq 'active (nngithub-retrieve-groups '("a/b.issues") "test")))
     (should-not (plist-get nngithub--state :groups)))))

(ert-deftest nngithub-scan-honors-gnus-subscriptions ()
  (nngithub-test--isolated
   (let ((gnus-newsrc-alist
          '((dummy)
            ("nngithub+test:a/b.issues" 3 nil nil (nngithub "test"))
            ("nngithub+test:a/b.pulls" 6 nil nil (nngithub "test"))
            ("nngithub+other:c/d.issues" 3 nil nil (nngithub "other"))))
         scanned)
     (cl-letf (((symbol-function 'gnus-find-method-for-group)
                (lambda (name) (nth 4 (assoc name gnus-newsrc-alist))))
               ((symbol-function 'nngithub--sync)
                (lambda (group &rest _) (push group scanned))))
       (should (nngithub-request-scan nil "test"))
       (should (equal scanned '("a/b.issues")))))))

(ert-deftest nngithub-scan-continues-after-group-error ()
  (nngithub-test--isolated
   (let (scanned)
     (cl-letf (((symbol-function 'nngithub--subscribed-groups)
                (lambda () '("a/b.issues" "c/d.issues")))
               ((symbol-function 'nngithub--sync)
                (lambda (group &rest _)
                  (push group scanned)
                  (when (equal group "a/b.issues") (error "Unavailable")))))
       (should-not (nngithub-request-scan nil "test"))
       (should (equal scanned '("c/d.issues" "a/b.issues")))
       (should (string-match-p "a/b.issues: Unavailable"
                               (nngithub-status-message)))))))

(ert-deftest nngithub-api-guards-reentrant-server-state ()
  (nngithub-test--isolated
   (let ((nngithub--busy t))
     (should-error (nngithub-open-server "other"))
     (should-error (nngithub--api "query { viewer { login } }"))
     (should (equal "test" (nnoo-current-server 'nngithub))))))

(ert-deftest nngithub-reply-finds-server-created-in-gnus ()
  (nngithub-test--isolated
   (let* ((article (nngithub-test--seed "a/b.issues"))
          (gnus-server-alist '(("test" nngithub "test"))))
     (nngithub--save)
     (nngithub-open-server "other")
     (nngithub--load)
     (should (equal article
                    (cadr (nngithub--locate (plist-get article :message-id)))))
     (should (equal "test" (nnoo-current-server 'nngithub))))))

(ert-deftest nngithub-missing-parent-must-not-fall-back-to-root ()
  (nngithub-test--isolated
   (let* ((reply (nngithub-test--seed "a/b.discussions"))
          (state (nngithub--group "a/b.discussions")))
     (setf (plist-get state :articles)
           (list (car (plist-get state :articles))))
     (nngithub-test--message
      (concat "Newsgroups: a/b.discussions\nSubject: Re: title\nReferences: "
              (plist-get reply :references) " "
              (plist-get reply :message-id))
      (should-error (nngithub--post "a/b.discussions") :type 'user-error)))))

(ert-deftest nngithub-root-pagination ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'nngithub--api)
               (lambda (query &optional _vars)
                 (cl-incf calls)
                 (when (= calls 2) (should (string-match-p "after: \"cursor\"" query)))
                 `((target . ((page . ((nodes . ,(list (list (cons 'id calls))))
                                       (pageInfo . ((hasNextPage . ,(= calls 1))
                                                    (endCursor . "cursor")))))))))))
      (should (equal (mapcar (lambda (n) (alist-get 'id n))
                             (nngithub--connection "repository" "issues" nil "id")) '(1 2)))
      (should (= calls 2)))))

(ert-deftest nngithub-child-pagination-and-stuck-cursor ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'nngithub--api)
               (lambda (query &optional _vars)
                 (cl-incf calls)
                 (should (string-match-p "on DiscussionComment" query))
                 `((target . ((page . ((nodes . ,(list (list (cons 'id calls))))
                                       (pageInfo . ((hasNextPage . ,(= calls 1))
                                                    (endCursor . "cursor")))))))))))
      (should (= 2 (length (nngithub--children "ID" "DiscussionComment" "replies" "id"))))))
  (cl-letf (((symbol-function 'nngithub--api)
             (lambda (&rest _)
               '((target . ((page . ((pageInfo . ((hasNextPage . t) (endCursor . "same")))))))))))
    (should-error (nngithub--connection "repository" "issues" nil "id"))))

(ert-deftest nngithub-pagination-deduplicates-moving-nodes ()
  (let ((calls 0))
    (cl-letf (((symbol-function (quote nngithub--api))
               (lambda (&rest _)
                 (cl-incf calls)
                 (list (cons (quote target)
                             (list (cons (quote page)
                                         (list
                                          (cons (quote nodes) (list (list (cons (quote id) "same"))))
                                          (cons (quote pageInfo)
                                                (list (cons (quote hasNextPage) (= calls 1))
                                                      (cons (quote endCursor) "cursor")))))))))))
      (should (= 1 (length (nngithub--connection "repository" "issues" nil "id"))))
      (should (= 2 calls)))))

(defun nngithub-test--page (nodes &optional cursor)
  "Make a connection page of NODES, with optional continuation CURSOR."
  `((nodes . ,nodes)
    (pageInfo . ((hasNextPage . ,(and cursor t)) (endCursor . ,cursor)))))

(defun nngithub-test--response (page)
  "Wrap connection PAGE in a GraphQL response."
  `((target . ((page . ,page)))))

(ert-deftest nngithub-prefetched-page-is-complete-or-resumes ()
  (let ((first (nngithub-test--page (list (nngithub-test--node "FIRST"))))
        (calls 0))
    (cl-letf (((symbol-function 'nngithub--api)
               (lambda (&rest _) (ert-fail "Refetched a complete page"))))
      (should (= 1 (length (nngithub--children
                            "ROOT" "Issue" "comments" "id" first))))
      (should-not (nngithub--children
                   "ROOT" "Issue" "comments" "id" (nngithub-test--page nil))))
    (setf (alist-get 'pageInfo first)
          '((hasNextPage . t) (endCursor . "first-end")))
    (cl-letf (((symbol-function 'nngithub--api)
               (lambda (query &optional _variables)
                 (cl-incf calls)
                 (should (string-match-p "after: \"first-end\"" query))
                 (nngithub-test--response
                  (nngithub-test--page
                   (list (nngithub-test--node "FIRST")
                         (nngithub-test--node "SECOND")))))))
      (should (equal '("FIRST" "SECOND")
                     (mapcar (lambda (node) (alist-get 'id node))
                             (nngithub--children
                              "ROOT" "Issue" "comments" "id" first))))
      (should (= 1 calls)))
    (cl-letf (((symbol-function 'nngithub--api)
               (lambda (&rest _) (nngithub-test--response first))))
      (should-error (nngithub--children
                     "ROOT" "Issue" "comments" "id" first)))))

(ert-deftest nngithub-initial-sync-batches-roots-and-keeps-incremental-light ()
  (nngithub-test--isolated
   (let ((calls 0)
         (root (nngithub-test--node "ROOT")))
     (setf (alist-get 'comments root)
           (nngithub-test--page (list (nngithub-test--node "COMMENT"))))
     (cl-letf (((symbol-function 'nngithub--api)
                (lambda (query &optional _variables)
                  (cl-incf calls)
                  (pcase calls
                    (1 (should (string-match-p "issues(first: 20" query))
                       (should (string-match-p "comments(first: 20" query)))
                    (2 (should (string-match-p "after: \"root-end\"" query)))
                    (3 (should (string-match-p "issues(first: 100" query))
                       (should-not (string-match-p "comments(" query))))
                  (nngithub-test--response
                   (nngithub-test--page
                    (unless (= calls 2) (list root))
                    (when (= calls 1) "root-end"))))))
       (nngithub--sync "a/b.issues")
       (should (= 2 calls))
       (let ((articles (copy-tree
                        (plist-get (nngithub--group "a/b.issues") :articles))))
         (should (= 2 (length articles)))
         (nngithub--sync "a/b.issues")
         (should (= 3 calls))
         (should (equal articles
                        (plist-get (nngithub--group "a/b.issues") :articles))))))))

(ert-deftest nngithub-prefetched-discussions-follow-both-continuations ()
  (nngithub-test--isolated
   (let ((root (nngithub-test--node "ROOT"))
         (comment (nngithub-test--node "COMMENT"))
         (calls 0))
     (setf (alist-get 'replies comment)
           (nngithub-test--page (list (nngithub-test--node "REPLY")) "reply-end")
           (alist-get 'comments root)
           (nngithub-test--page (list comment) "comment-end"))
     (cl-letf (((symbol-function 'nngithub--api)
                (lambda (query &optional _variables)
                  (cl-incf calls)
                  (let ((page
                         (cond
                          ((string-match-p "after: \"comment-end\"" query)
                           (should (string-match-p "replies(first: 20" query))
                           (let ((next (nngithub-test--node "COMMENT2")))
                             (setf (alist-get 'replies next)
                                   (nngithub-test--page nil))
                             (nngithub-test--page (list next))))
                          ((string-match-p "after: \"reply-end\"" query)
                           (nngithub-test--page
                            (list (nngithub-test--node "REPLY2"))))
                          (t (ert-fail "Unnecessary child query")))))
                    (nngithub-test--response page)))))
       (let ((articles (nngithub--conversation root "a/b.discussions"
                                               "discussions")))
         (should (= 2 calls))
         (should (equal '("ROOT" "COMMENT" "REPLY" "REPLY2" "COMMENT2")
                        (mapcar (lambda (a) (plist-get a :id)) articles)))
         (should (equal "COMMENT" (plist-get (nth 3 articles) :reply))))))))

(ert-deftest nngithub-prefetched-reviews-and-threads-keep-all-pages ()
  (nngithub-test--isolated
   (let ((root (nngithub-test--node "ROOT"))
         (review (nngithub-test--node "REVIEW"))
         (thread (list (cons 'id "THREAD")))
         (calls 0))
     (setf (alist-get 'state review) "APPROVED"
           (alist-get 'comments thread)
           (nngithub-test--page (list (nngithub-test--node "INLINE")) "inline-end")
           (alist-get 'comments root) (nngithub-test--page nil)
           (alist-get 'reviews root) (nngithub-test--page (list review) "review-end")
           (alist-get 'reviewThreads root)
           (nngithub-test--page (list thread) "thread-end"))
     (cl-letf (((symbol-function 'nngithub--api)
                (lambda (query &optional _variables)
                  (cl-incf calls)
                  (let ((page
                         (cond
                          ((string-match-p "after: \"review-end\"" query)
                           (let ((pending (nngithub-test--node "PENDING")))
                             (setf (alist-get 'state pending) "PENDING")
                             (nngithub-test--page (list pending))))
                          ((string-match-p "after: \"thread-end\"" query)
                           (should (string-match-p "comments(first: 20" query))
                           (nngithub-test--page
                            (list `((id . "THREAD2")
                                    (comments . ,(nngithub-test--page nil))))))
                          ((string-match-p "after: \"inline-end\"" query)
                           (nngithub-test--page
                            (list (nngithub-test--node "INLINE2"))))
                          (t (ert-fail "Unnecessary child query")))))
                    (nngithub-test--response page)))))
       (let ((articles (nngithub--conversation root "a/b.pulls" "pulls")))
         (should (= 3 calls))
         (should (equal '("ROOT" "REVIEW" "INLINE" "INLINE2")
                        (mapcar (lambda (a) (plist-get a :id)) articles)))
         (should (equal "THREAD" (plist-get (nth 3 articles) :review)))
         (should (string-match-p (regexp-quote (nngithub--id "a/b.pulls" "INLINE"))
                                 (plist-get (nth 3 articles) :references))))))))

(ert-deftest nngithub-stable-numbers-deletion-restart-and-permissions ()
  (nngithub-test--isolated
   (let ((roots (list (nngithub-test--node "ROOT")))
         (comments (list (nngithub-test--node "C1" "2026-01-03T00:00:00Z")))
         (fetches 0))
     (cl-letf (((symbol-function 'nngithub--connection) (lambda (&rest _) roots))
               ((symbol-function 'nngithub--children)
                (lambda (&rest _) (cl-incf fetches) comments)))
       (nngithub--sync "a/b.issues")
       (should (= 1 fetches))
       (should (equal "C1" (plist-get (nngithub--find 2 "a/b.issues") :id)))
       (nngithub--sync "a/b.issues")
       (should (= 1 fetches))
       (setq comments (list (nngithub-test--node "C2" "2025-01-01T00:00:00Z")))
       (nngithub--sync "a/b.issues" t)
       (should-not (nngithub--find 2 "a/b.issues"))
       (should (equal "ROOT" (plist-get (nngithub--find 1 "a/b.issues") :id)))
       (should (equal "C2" (plist-get (nngithub--find 3 "a/b.issues") :id)))
       (should (= #o600 (logand #o777 (file-modes (nngithub--file)))))
       (setq nngithub--loaded nil nngithub--state nil)
       (nngithub--load)
       (should (= 4 (plist-get (nngithub--group "a/b.issues") :next)))
       (should (equal "C2" (plist-get (nngithub--find 3 "a/b.issues") :id)))))))

(ert-deftest nngithub-sync-errors-preserve-cache ()
  (nngithub-test--isolated
   (nngithub-test--seed "a/b.issues")
   (nngithub--save)
   (let ((before (copy-tree nngithub--state)))
     (cl-letf (((symbol-function 'nngithub--connection)
                (lambda (&rest _) (list (nngithub-test--node "NEW"))))
               ((symbol-function 'nngithub--children) (lambda (&rest _) (error "Rate limited"))))
       (should-error (nngithub--sync "a/b.issues"))
       (should (equal before nngithub--state)))
     (cl-letf (((symbol-function 'nngithub--connection) (lambda (&rest _) nil))
               ((symbol-function 'nngithub--save) (lambda () (error "Disk full"))))
       (should-error (nngithub--sync "a/b.issues"))
       (should (equal before nngithub--state))))))

(ert-deftest nngithub-discussion-threading ()
  (nngithub-test--isolated
   (cl-letf (((symbol-function 'nngithub--children)
              (lambda (_id _type field _fields &optional _page)
                (list (nngithub-test--node (if (equal field "comments") "COMMENT" "REPLY"))))))
     (let* ((articles (nngithub--conversation (nngithub-test--node "ROOT") "a/b.discussions" "discussions"))
            (reply (nth 2 articles)))
       (should (= 3 (length articles)))
       (should (equal "COMMENT" (plist-get reply :reply)))
       (should (equal (concat (nngithub--id "a/b.discussions" "ROOT") " "
                              (nngithub--id "a/b.discussions" "COMMENT"))
                      (plist-get reply :references)))))))

(ert-deftest nngithub-review-threading-and-pending-reviews ()
  (nngithub-test--isolated
   (cl-letf (((symbol-function 'nngithub--children)
              (lambda (_id type field _fields &optional _page)
                (cond ((equal field "reviews")
                       (list (append '((state . "PENDING")) (nngithub-test--node "REVIEW"))))
                      ((equal field "reviewThreads") '(((id . "THREAD"))))
                      ((equal type "PullRequestReviewThread")
                       (list (append '((path . "file.el") (diffHunk . "+new"))
                                     (nngithub-test--node "INLINE"))))))))
     (let ((articles (nngithub--conversation (nngithub-test--node "ROOT") "a/b.pulls" "pulls")))
       (should (= 2 (length articles)))
       (should (equal "THREAD" (plist-get (nth 1 articles) :review)))
       (should (string-match-p "file.el\n```diff" (plist-get (nth 1 articles) :body)))))))

(ert-deftest nngithub-backend-headers-and-article ()
  (nngithub-test--isolated
   (let ((reply (nngithub-test--seed "a/b.issues")))
     (should (nngithub-request-group "a/b.issues" "test" t))
     (with-current-buffer nntp-server-buffer
       (should (string-match-p "211 2 1 2 a/b.issues" (buffer-string))))
     (should (eq 'nov (nngithub-retrieve-headers '(1 . 2) "a/b.issues" "test")))
     (with-current-buffer nntp-server-buffer (should (= 2 (count-lines (point-min) (point-max)))))
     (should (equal '("a/b.issues" . 2)
                    (nngithub-request-article (plist-get reply :message-id) "a/b.issues" "test")))
     (with-current-buffer nntp-server-buffer
       (should (equal (substring (plist-get reply :message-id) 1 -1)
                      (message-fetch-field "Reply-To")))
       (should (string-match-p "Hello 世界" (buffer-string)))))))

(ert-deftest nngithub-headers-preserve-request-order-and-skip-holes ()
  (nngithub-test--isolated
   (nngithub-test--seed "a/b.issues")
   (let ((first (plist-get (nngithub--find 1 "a/b.issues") :message-id)))
     (should (eq 'nov (nngithub-retrieve-headers
                       (list 2 999 first) "a/b.issues" "test")))
     (with-current-buffer nntp-server-buffer
       (should (equal '("2" "1")
                      (mapcar (lambda (line) (car (split-string line "\t")))
                              (split-string (buffer-string) "\n" t))))))))

(ert-deftest nngithub-all-reply-mutations ()
  (let (calls)
    (cl-letf (((symbol-function 'nngithub--api)
               (lambda (query variables)
                 (push (cons query variables) calls)
                 (cond ((string-match-p "addDiscussionComment" query)
                        '((addDiscussionComment . ((comment . ((id . "NEW")))))))
                       ((string-match-p "addPullRequestReviewThreadReply" query)
                        '((addPullRequestReviewThreadReply . ((comment . ((id . "NEW")))))))
                       (t '((addComment . ((commentEdge . ((node . ((id . "NEW")))))))))))))
      (dolist (kind '("issues" "pulls" "discussions"))
        (should (nngithub--reply '(:root "ROOT" :reply "COMMENT") "世界" (concat "a/b." kind))))
      (should (equal "COMMENT" (nngithub--get (cdar calls) 'input 'replyToId)))
      (should (nngithub--reply '(:root "ROOT" :review "THREAD") "text" "a/b.pulls"))
      (should (equal "THREAD" (nngithub--get (cdar calls) 'input 'pullRequestReviewThreadId)))
      (should (= 4 (length calls))))))

(ert-deftest nngithub-reply-send-routing-and-mixed-recipients ()
  (nngithub-test--isolated
   (let* ((article (nngithub-test--seed "a/b.issues"))
          (address (substring (plist-get article :message-id) 1 -1)) sent)
     (cl-letf (((symbol-function 'nngithub--reply)
                (lambda (target body group) (setq sent (list target body group)))))
       (nngithub-test--message (concat "To: " address "\nSubject: Re: test")
                               (nngithub--prepare-send)
                               (should (equal '((nngithub always nngithub--send-reply)) message-send-method-alist))
                               (should (nngithub--send-reply))
                               (should (equal article (car sent)))
                               (should (equal "Reply 世界\n" (cadr sent)))
                               (should (memq 'nngithub message-sent-message-via)))
       (setq sent nil)
       (nngithub-test--message (concat "To: " address "\nCc: other@example.com\nSubject: Re: test")
                               (should-error (nngithub--send-reply) :type 'user-error)
                               (should-not sent))
       (nngithub-test--message "To: author@nngithub.invalid\nSubject: test"
                               (should-error (nngithub--prepare-send) :type 'user-error))))))

(ert-deftest nngithub-normal-mail-untouched ()
  (nngithub-test--message "To: friend@example.com\nSubject: test"
                          (let ((before message-send-method-alist))
                            (nngithub--prepare-send)
                            (should (eq before message-send-method-alist)))))

(ert-deftest nngithub-message-send-end-to-end ()
  (nngithub-test--isolated
   (let* ((article (nngithub-test--seed "a/b.issues"))
          (address (substring (plist-get article :message-id) 1 -1))
          (message-confirm-send nil)
          (message-interactive nil)
          (message-send-hook '(nngithub--prepare-send))
          sent)
     (cl-letf (((symbol-function 'nngithub--reply)
                (lambda (_article body _group) (setq sent body)))
               ((symbol-function 'message-send-mail) (lambda (&rest _) (ert-fail "SMTP attempted")))
               ((symbol-function 'message-send-news) (lambda (&rest _) (ert-fail "NNTP attempted"))))
       (nngithub-test--message (concat "To: " address "\nSubject: Re: test\nFrom: me@example.com")
                               (run-hooks 'message-setup-hook)
                               (message-send)
                               (should (equal sent "Reply 世界\n"))
                               (should (memq 'nngithub message-sent-message-via)))))))

(ert-deftest nngithub-api-process-json-errors-and-timeout ()
  (let ((script (make-temp-file "nngithub-gh-"))
        (nngithub-timeout 3))
    (unwind-protect
        (let ((nngithub-program script))
          (with-temp-file script
            (insert "#!/bin/sh\ncat >/dev/null\nprintf '%s' '{\"data\":{\"value\":\"世界\"}}'\n"))
          (set-file-modes script #o700)
          (should (equal '((value . "世界")) (nngithub--api "query { value }")))
          (with-temp-file script
            (insert "#!/bin/sh\ncat >/dev/null\nprintf '%s' '{\"data\":{\"value\":1},\"errors\":[{\"message\":\"denied\"}]}'\n"))
          (should-error (nngithub--api "query { value }"))
          (with-temp-file script
            (insert "#!/bin/sh\ncat >/dev/null\nprintf 'permission denied' >&2\nexit 1\n"))
          (should-error (nngithub--api "query { value }"))
          (with-temp-file script (insert "#!/bin/sh\ncat >/dev/null\nexec sleep 3\n"))
          (let ((nngithub-timeout 0))
            (should-error (nngithub--api "query { value }"))))
      (delete-file script))))

(ert-deftest nngithub-close-cancels-api-process ()
  (dolist (close '(nngithub-close-server gnus-close-backends))
    (nngithub-test--isolated
     (let ((script (make-temp-file "nngithub-gh-"))
           (nngithub-timeout 3)
           (gnus-valid-select-methods '(("nngithub")))
           timer process stderr output errors cancelled)
       (unwind-protect
           (let ((nngithub-program script))
             (with-temp-file script
               (insert "#!/bin/sh\ncat >/dev/null\nexec sleep 30\n"))
             (set-file-modes script #o700)
             (setq timer
                   (run-at-time
                    0.05 nil
                    (lambda ()
                      (setq process nngithub--busy
                            stderr (process-get process 'stderr)
                            output (process-buffer process)
                            errors (process-buffer stderr))
                      (funcall close))))
             (condition-case nil
                 (nngithub--api "query { value }")
               (quit (setq cancelled t)))
             (should cancelled)
             (should-not (process-live-p process))
             (should-not (process-live-p stderr))
             (should-not (buffer-live-p output))
             (should-not (buffer-live-p errors))
             (should-not nngithub--busy)
             (should (nngithub-open-server "test"))
             (with-temp-file script
               (insert "#!/bin/sh\ncat >/dev/null\nprintf '%s' '{\"data\":{\"value\":1}}'\n"))
             (should (equal '((value . 1)) (nngithub--api "query { value }"))))
         (when timer (cancel-timer timer))
         (delete-file script))))))

(ert-deftest nngithub-close-cancels-worker-process ()
  (let ((script (make-temp-file "nngithub-gh-"))
        (gnus-valid-select-methods '(("nngithub")))
        worker process stderr cancelled)
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/bin/sh\ncat >/dev/null\nexec sleep 30\n"))
          (set-file-modes script #o700)
          (setq worker
                (make-thread
                 (lambda ()
                   (let ((nngithub-program script) (nngithub-timeout 3))
                     (condition-case nil
                         (nngithub--api "query { value }")
                       (quit (setq cancelled t)))))))
          (let ((deadline (+ (float-time) 3)))
            (while (and (not (processp nngithub--busy))
                        (thread-live-p worker) (< (float-time) deadline))
              (sleep-for 0.01)))
          (should (processp nngithub--busy))
          (setq process nngithub--busy stderr (process-get process 'stderr))
          (gnus-close-backends)
          (should-not (process-live-p process))
          (should-not (process-live-p stderr))
          (thread-join worker)
          (should cancelled)
          (should-not nngithub--busy))
      (when (and worker (thread-live-p worker))
        (thread-signal worker 'quit nil)
        (thread-join worker))
      (delete-file script))))

(ert-deftest nngithub-followup-raw-unicode-routing ()
  (nngithub-test--isolated
   (let ((parent (nngithub-test--seed "a/b.issues")) sent)
     (cl-letf (((symbol-function 'nngithub--reply)
                (lambda (target body group) (setq sent (list target body group)))))
       (nngithub-test--message (concat "Newsgroups: a/b.issues\nSubject: Re: title\nReferences: "
                                       (plist-get parent :message-id))
                               (setq-local message-post-method '(nngithub "test"))
                               (nngithub--prepare-send)
                               (should (equal '((nngithub always nngithub--send-post)) message-send-method-alist))
                               (should (nngithub--send-post))
                               (should (equal "Reply 世界\n" (cadr sent)))
                               (should (equal parent (car sent))))))))

(ert-deftest nngithub-missing-reference-must-not-create-issue ()
  (nngithub-test--isolated
   (nngithub-test--message "Newsgroups: a/b.issues\nSubject: Re: title\nReferences: <missing@nngithub.invalid>"
                           (should-error (nngithub--post "a/b.issues") :type 'user-error))))

(ert-deftest nngithub-mime-rejected-before-writing ()
  (dolist (body '("<#part type=application/octet-stream>\nfile" "<#secure method=pgpmime>" "   "))
    (nngithub-test--message "Subject: test"
                            (message-goto-body)
                            (delete-region (point) (point-max))
                            (insert body)
                            (should-error (nngithub--body) :type 'user-error))))

(ert-deftest nngithub-new-conversations ()
  (nngithub-test--isolated
   (let (calls)
     (cl-letf (((symbol-function 'nngithub--api) (lambda (&rest _) '((repository . ((id . "REPO"))))))
               ((symbol-function 'nngithub--children) (lambda (&rest _) '(((id . "CAT") (name . "General")))))
               ((symbol-function 'nngithub--mutation)
                (lambda (name _type input _result) (push (cons name input) calls))))
       (nngithub-test--message "Subject: 世界\nX-GitHub-Category: General\nX-GitHub-Base: main\nX-GitHub-Head: me:branch"
                               (dolist (kind '("issues" "discussions" "pulls"))
                                 (nngithub--new (concat "a/b." kind) (nngithub--body))))
       (should (equal '("createPullRequest" "createDiscussion" "createIssue") (mapcar #'car calls)))
       (should (equal "me:branch" (alist-get 'headRefName (cdar calls))))
       (should (equal "CAT" (alist-get 'categoryId (cdr (nth 1 calls)))))
       (nngithub-test--message "Subject: test"
                               (should-error (nngithub--new "a/b.pulls" "body") :type 'user-error)
                               (should-error (nngithub--new "a/b.discussions" "body") :type 'user-error))))))

(ert-deftest nngithub-server-state-isolation-and-draft-recovery ()
  (nngithub-test--isolated
   (let* ((first (nngithub-test--seed "a/b.issues"))
          (id (plist-get first :message-id))
          (gnus-secondary-select-methods '((nngithub "test") (nngithub "second"))))
     (nngithub--save)
     (nngithub-open-server "second" '((nngithub-host "enterprise.example")))
     (nngithub--load)
     (should-not (plist-get nngithub--state :groups))
     (nngithub-test--seed "a/b.issues")
     (should-not (equal id (plist-get (nngithub--find 2 "a/b.issues") :message-id)))
     (should (equal first (cadr (nngithub--locate id))))
     (should (equal "github.com" nngithub-host)))))

(provide 'nngithub-tests)
;;; nngithub-tests.el ends here
