;;; nngithub.el --- GitHub conversations in Gnus -*- lexical-binding: t; -*-

;; Assisted-By: Codex:gpt-6-astra
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: news, comm
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Read and participate in GitHub issues, pull requests and discussions with
;; Gnus.  Requires the GitHub CLI, authenticated with `gh auth login'.  The
;; selected gh account needs read access, and write access to comment/create.
;; GitHub Enterprise is supported through nngithub-host.
;;
;; (use-package nngithub
;;   :ensure nil
;;   :load-path "lisp/nngithub"
;;   :demand t)
;;
;; Add the server in Gnus: ^, a, nngithub, github.com.
;; Alternatively, add (nngithub "github.com") to
;; `gnus-secondary-select-methods'.  No repository list belongs in the config.
;;
;; Each repository exposes OWNER/REPO.issues, OWNER/REPO.pulls and
;; OWNER/REPO.discussions.  Press RET on the server to browse groups for your
;; watched repositories, then u on a group to subscribe.  GitHub's entire
;; repository namespace cannot be enumerated: for any other repository, use
;; G m in the group buffer, enter OWNER/REPO.issues (or .pulls/.discussions),
;; and select the nngithub server.  This registers a local Gnus group; it
;; does not create anything on GitHub.  Gnus owns subscriptions and read marks.
;; Only subscribed groups are scanned; browsing does not import their history.
;; GitHub notification settings are untouched.
;; `g' scans for updates.  `r'/`R' and `f'/`F' compose comments; C-c C-c
;; publishes through the API, never SMTP.  Replies to discussion comments
;; and inline review comments stay in their respective GitHub threads.
;; Bodies are Markdown, displayed as plain text, including review diff hunks.
;; `a' creates an issue, discussion or PR.  New discussions require an
;; X-GitHub-Category header (category name or node ID); PRs require
;; X-GitHub-Base and X-GitHub-Head (already pushed branches, owner:branch OK).
;; `nngithub-compose' prompts for these fields.  Attachments and encrypted
;; messages are rejected: upload files to GitHub and paste their URLs instead.
;; `nngithub-browse' opens the current article on GitHub.
;;
;; Initial sync imports all history in batches, including the first pages of
;; comments, replies and reviews.  Longer threads are paginated separately.
;; Later scans fetch comments only for changed conversations.
;; `nngithub-rescan' forces a full refresh (including comment edits/deletions).
;; Scanning is synchronous but cancellable with C-g, with a timeout per API
;; call.  API failures preserve the previous snapshot and report an error.
;; Closing the server or quitting Gnus terminates any pending gh request.
;; Cached articles remain readable offline; scans still report network errors.
;; Deleted articles disappear after their conversation is refreshed; article
;; numbers are never recycled.  Back up the cache along with .newsrc.eld:
;; deleting it loses the mapping between article numbers and Gnus read marks.
;; Cache files contain private repository content and are written mode 0600.
;; Do not share a cache directory between concurrent Emacs processes.
;;
;; This is a conversation backend, not a repository administration client:
;; merging, approvals, labels, reactions and moderation use the GitHub UI.
;; Requests are not automatically retried, especially writes.  If a write
;; times out or is cancelled, check GitHub before resending to avoid a
;; duplicate comment.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'nnoo)
(require 'nnheader)
(require 'gnus)
(require 'gnus-start)
(require 'gnus-group)
(require 'gnus-sum)
(require 'gnus-msg)
(require 'message)
(require 'mail-parse)

(defgroup nngithub nil "GitHub conversations in Gnus." :group 'gnus)

(defcustom nngithub-directory (expand-file-name "nngithub/" gnus-directory)
  "Persistent article cache.  Back this up with your Gnus state."
  :type 'directory)
(defcustom nngithub-program "gh"
  "GitHub CLI executable.  Authentication is managed by gh."
  :type 'string)
(defcustom nngithub-timeout 90
  "Maximum seconds for one API call.  No automatic retries are made."
  :type 'natnum)

(nnoo-declare nngithub)
(defvoo nngithub-host "github.com" "GitHub host, as used by gh auth login.")
(defvoo nngithub--state nil)
(defvoo nngithub--loaded nil)
(nnoo-define-basics nngithub)

(defvar nngithub--busy nil
  "Active GitHub process, t during setup, or nil when idle.
Shared across threads so server shutdown can cancel a pending request.")

(deffoo nngithub-open-server (server &optional defs)
  (when (and nngithub--busy
             (not (nnoo-current-server-p 'nngithub server)))
    (error "Cannot switch GitHub servers during an API request"))
  (nnoo-change-server 'nngithub server defs))

(deffoo nngithub-close-server (&optional server _defs)
  (when (and (processp nngithub--busy)
             (equal (or server (nnoo-current-server 'nngithub))
                    (process-get nngithub--busy 'server)))
    (process-put nngithub--busy 'cancelled t)
    (let ((stderr (process-get nngithub--busy 'stderr)))
      (when (process-live-p nngithub--busy) (delete-process nngithub--busy))
      (when (and stderr (process-live-p stderr)) (delete-process stderr))))
  (nnoo-close-server 'nngithub server))

(deffoo nngithub-request-close ()
  (nngithub-close-server
   (when (processp nngithub--busy) (process-get nngithub--busy 'server)))
  (nnoo-close 'nngithub))

(defconst nngithub--fields "id body createdAt updatedAt url author { login }")
(defconst nngithub--review-fields
  "id body createdAt url author { login } state"
  "Fields for a pull request review.")
(defconst nngithub--review-comment-fields
  (concat nngithub--fields " path diffHunk")
  "Fields for an inline review comment.")
(defconst nngithub--kinds
  '(("issues" "issues" "Issue") ("pulls" "pullRequests" "PullRequest")
    ("discussions" "discussions" "Discussion")))

(defun nngithub--get (object &rest keys)
  "Get nested KEYS from JSON OBJECT."
  (dolist (key keys object) (setq object (alist-get key object))))

(defun nngithub--api (query &optional variables)
  "Execute GraphQL QUERY with VARIABLES through gh, without shell expansion."
  (when nngithub--busy (error "A GitHub API request is already in progress"))
  (unless (executable-find nngithub-program)
    (error "Install GitHub CLI and run gh auth login first"))
  (let ((output (generate-new-buffer " *nngithub-api*"))
        (errors (generate-new-buffer " *nngithub-errors*"))
        (coding-system-for-write 'utf-8-unix)
        process stderr-process)
    (unwind-protect
        (progn
          (setq nngithub--busy t)
          (setq process
                (make-process
                 :name "nngithub-api" :buffer output :stderr errors
                 :connection-type 'pipe :coding 'utf-8-unix :noquery t :sentinel #'ignore
                 :command (list nngithub-program "api" "graphql" "--hostname"
                                nngithub-host "--input" "-")))
          (setq stderr-process (get-buffer-process errors))
          (process-put process 'server (nnoo-current-server 'nngithub))
          (process-put process 'stderr stderr-process)
          (setq nngithub--busy process)
          (set-process-sentinel stderr-process #'ignore)
          (process-send-string
           process (json-encode `((query . ,query)
                                  (variables . ,(or variables (make-hash-table))))))
          (process-send-eof process)
          (let ((deadline (+ (float-time) nngithub-timeout)))
            ;; Exit status alone does not imply that both pipes are drained.
            (while (or (process-live-p process)
                       (process-live-p stderr-process)
                       (accept-process-output process 0)
                       (accept-process-output stderr-process 0))
              (when (>= (float-time) deadline)
                (error "GitHub request timed out; check GitHub before resending writes"))
              (accept-process-output process 0.05)
              (accept-process-output stderr-process 0.05)))
          (when (process-get process 'cancelled) (signal 'quit nil))
          (unless (zerop (process-exit-status process))
            (error "GitHub API: %s" (with-current-buffer errors
                                      (string-trim (buffer-string)))))
          (let* ((json (with-current-buffer output
                         (json-parse-string (buffer-string) :object-type 'alist
                                            :array-type 'list :null-object nil
                                            :false-object nil)))
                 (failures (alist-get 'errors json)))
            (when failures
              (error "GitHub GraphQL: %s"
                     (mapconcat (lambda (e) (alist-get 'message e)) failures "; ")))
            (or (alist-get 'data json) (error "GitHub returned no data"))))
      (when (and process (process-live-p process)) (delete-process process))
      (when (and stderr-process (process-live-p stderr-process))
        (delete-process stderr-process))
      (kill-buffer output)
      (kill-buffer errors)
      (setq nngithub--busy nil))))

(defun nngithub--connection (prefix field arguments fields
                                    &optional type page page-size)
  "Read every page of FIELD under PREFIX with ARGUMENTS and node FIELDS.
PREFIX must select its parent as `target'.
When TYPE is non-nil, wrap the connection in a GraphQL inline fragment.
Consume prefetched PAGE before making requests, continuing from its cursor.
PAGE-SIZE defaults to 100; smaller batches bound nested query sizes."
  (let ((more t) (seen (make-hash-table :test #'equal)) cursor result)
    (while more
      (unless page
        (let ((query (format
                      (concat "query { %s { %s page: %s(first: %d%s%s) { "
                              "nodes { %s } pageInfo { hasNextPage endCursor } "
                              "} %s } }")
                      prefix (if type (format "... on %s {" type) "")
                      field (or page-size 100)
                      (if arguments (concat ", " arguments) "")
                      (if cursor (concat ", after: " (json-encode cursor)) "")
                      fields (if type "}" ""))))
          (setq page (nngithub--get (nngithub--api query) 'target 'page))))
      (let* ((info (alist-get 'pageInfo page))
             (next (alist-get 'endCursor info)))
        (unless page (error "GitHub connection unavailable: %s" field))
        (dolist (node (alist-get 'nodes page))
          (let ((id (or (alist-get 'id node) (alist-get 'nameWithOwner node))))
            (unless (and id (gethash id seen))
              (when id (puthash id t seen))
              (push node result))))
        (setq more (alist-get 'hasNextPage info))
        (when (and more (or (null next) (equal next cursor)))
          (error "GitHub returned a non-advancing pagination cursor"))
        (setq cursor next page nil)))
    (nreverse result)))

(defun nngithub--children (id type field fields &optional page)
  "Read FIELD of node ID of TYPE, selecting FIELDS, starting with PAGE."
  (nngithub--connection (format "target: node(id: %s)" (json-encode id))
                        field nil fields type page))

(defun nngithub--first-page (field fields)
  "Select the first page of FIELD with node FIELDS and its continuation.
Twenty nodes per nested connection keeps the query size bounded."
  (format (concat "%s(first: 20) { nodes { %s } "
                  "pageInfo { hasNextPage endCursor } }")
          field fields))

(defun nngithub--comment-fields (kind)
  "Select comment fields, including discussion replies, for KIND."
  (concat nngithub--fields
          (when (equal kind "discussions")
            (concat " " (nngithub--first-page "replies" nngithub--fields)))))

(defun nngithub--thread-fields ()
  "Select review thread fields, including the first page of comments."
  (concat "id " (nngithub--first-page
                 "comments" nngithub--review-comment-fields)))

(defun nngithub--conversation-fields (kind)
  "Select the first pages of all child connections for KIND."
  (concat
   (nngithub--first-page "comments" (nngithub--comment-fields kind))
   (when (equal kind "pulls")
     (concat " " (nngithub--first-page "reviews" nngithub--review-fields)
             " " (nngithub--first-page "reviewThreads"
                                       (nngithub--thread-fields))))))

(defun nngithub--parse-group (group)
  "Validate GROUP and return (OWNER REPO KIND)."
  (unless (and (stringp group)
               (string-match "\\`\\([[:alnum:]_-]+\\)/\\([[:alnum:]_.-]+\\)\\.\\(issues\\|pulls\\|discussions\\)\\'" group))
    (error "Expected OWNER/REPO.issues, .pulls or .discussions: %s" group))
  (list (match-string 1 group) (match-string 2 group) (match-string 3 group)))

(defun nngithub--file ()
  "Cache filename for the selected server and host."
  (expand-file-name (concat (secure-hash 'sha256
                                         (format "%s/%s" (nnoo-current-server 'nngithub)
                                                 nngithub-host)) ".eld")
                    nngithub-directory))

(defun nngithub--load ()
  "Load server state, rejecting unknown cache versions."
  (unless nngithub--loaded
    (setq nngithub--state
          (if (file-exists-p (nngithub--file))
              (with-temp-buffer
                (insert-file-contents (nngithub--file))
                (let ((data (read (current-buffer))))
                  (unless (and (listp data) (eq (plist-get data :version) 1))
                    (error "Unsupported nngithub cache; preserve it and investigate"))
                  data))
            (list :version 1 :groups nil)))
    (setq nngithub--loaded t)))

(defun nngithub--save ()
  "Atomically save state without exposing private repository bodies."
  (make-directory nngithub-directory t)
  (let ((temporary (make-temp-file (expand-file-name ".nngithub-" nngithub-directory)))
        (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil) (print-level nil))
              (prin1 nngithub--state (current-buffer))))
          (set-file-modes temporary #o600)
          (rename-file temporary (nngithub--file) t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun nngithub--select (&optional server)
  "Select SERVER and load its cache."
  (when (and server (not (nnoo-current-server-p 'nngithub server)))
    (nngithub-open-server server))
  (unless (nnoo-current-server 'nngithub) (nngithub-open-server "github"))
  (nngithub--load))

(defun nngithub--group (group)
  "Return cached GROUP state, creating an empty state if needed."
  (nngithub--parse-group group)
  (or (cdr (assoc group (plist-get nngithub--state :groups)))
      (let ((state (list :next 1 :articles nil :roots nil :synced nil)))
        (push (cons group state) (plist-get nngithub--state :groups))
        state)))

(defun nngithub--id (group id)
  "Stable RFC Message-ID for GROUP and GitHub node ID."
  (format "<%s@nngithub.invalid>"
          (secure-hash 'sha256 (format "%s/%s/%s/%s"
                                       (nnoo-current-server 'nngithub)
                                       nngithub-host group id))))

(defun nngithub--article (node root group &optional parent reply review)
  "Convert NODE and ROOT into an article in GROUP.
PARENT controls threading; REPLY and REVIEW route nested replies."
  (list :number nil :id (alist-get 'id node) :root (alist-get 'id root)
        :message-id (nngithub--id group (alist-get 'id node))
        :references (when parent
                      (mapconcat (lambda (id) (nngithub--id group id))
                                 (delete-dups (list (alist-get 'id root) parent)) " "))
        :reply reply :review review
        :subject (format "%s[%s #%s] %s" (if parent "Re: " "")
                         (upcase (nth 2 (nngithub--parse-group group)))
                         (alist-get 'number root) (alist-get 'title root))
        :author (or (nngithub--get node 'author 'login) "ghost")
        :date (alist-get 'createdAt node) :url (alist-get 'url node)
        :state (alist-get 'state root)
        :body (concat (when (alist-get 'path node)
                        (format "%s\n```diff\n%s\n```\n\n"
                                (alist-get 'path node) (or (alist-get 'diffHunk node) "")))
                      (or (alist-get 'body node) ""))))

(defun nngithub--conversation (root group kind)
  "Fetch ROOT and all its comments for GROUP of KIND."
  (let* ((id (alist-get 'id root))
         (type (nth 2 (assoc kind nngithub--kinds)))
         (articles (list (nngithub--article root root group))))
    (dolist (comment (nngithub--children
                      id type "comments" (nngithub--comment-fields kind)
                      (alist-get 'comments root)))
      (let ((cid (alist-get 'id comment)))
        (push (nngithub--article comment root group id
                                 (and (equal kind "discussions") cid)) articles)
        (when (equal kind "discussions")
          (dolist (reply (nngithub--children
                          cid "DiscussionComment" "replies" nngithub--fields
                          (alist-get 'replies comment)))
            (push (nngithub--article reply root group cid cid) articles)))))
    (when (equal kind "pulls")
      (dolist (review (nngithub--children
                       id "PullRequest" "reviews" nngithub--review-fields
                       (alist-get 'reviews root)))
        (unless (equal (alist-get 'state review) "PENDING")
          (let ((article (nngithub--article review root group id)))
            (setf (plist-get article :body)
                  (concat "Review: " (alist-get 'state review) "\n\n"
                          (plist-get article :body)))
            (push article articles))))
      (dolist (thread (nngithub--children
                       id "PullRequest" "reviewThreads"
                       (nngithub--thread-fields) (alist-get 'reviewThreads root)))
        (let ((tid (alist-get 'id thread)) parent)
          (dolist (comment (nngithub--children
                            tid "PullRequestReviewThread" "comments"
                            nngithub--review-comment-fields
                            (alist-get 'comments thread)))
            (push (nngithub--article comment root group (or parent id) nil tid) articles)
            (unless parent (setq parent (alist-get 'id comment)))))))
    (nreverse articles)))

(defun nngithub--sync (group &optional force)
  "Refresh GROUP transactionally.  FORCE refreshes unchanged conversations."
  (pcase-let* ((`(,owner ,repo ,kind) (nngithub--parse-group group))
               (state (nngithub--group group))
               (old (plist-get state :articles))
               (prefetch (or force (null (plist-get state :roots))))
               (roots (nngithub--connection
                       (format "target: repository(owner: %s, name: %s)"
                               (json-encode owner) (json-encode repo))
                       (nth 1 (assoc kind nngithub--kinds))
                       "orderBy: {field: CREATED_AT, direction: ASC}"
                       (concat nngithub--fields " number title "
                               (unless (equal kind "discussions") "state ")
                               (when prefetch
                                 (nngithub--conversation-fields kind)))
                       nil nil (if prefetch 20 100)))
               (next (plist-get state :next))
               (numbers (make-hash-table :test #'equal))
               (by-root (make-hash-table :test #'equal))
               (old-stamps (make-hash-table :test #'equal))
               (articles nil) (stamps nil))
    (dolist (article old)
      (puthash (plist-get article :id) (plist-get article :number) numbers)
      (push article (gethash (plist-get article :root) by-root)))
    (dolist (entry (plist-get state :roots))
      (puthash (car entry) (cdr entry) old-stamps))
    (dolist (root roots)
      (let* ((id (alist-get 'id root))
             (stamp (alist-get 'updatedAt root))
             (unchanged (and (not force)
                             (equal stamp (gethash id old-stamps)))))
        (push (cons id stamp) stamps)
        (dolist (article (if unchanged
                             (nreverse (gethash id by-root))
                           (nngithub--conversation root group kind)))
          (push article articles))))
    (setq articles (sort (nreverse articles) (lambda (a b)
                                               (string< (or (plist-get a :date) "")
                                                        (or (plist-get b :date) "")))))
    (dolist (article articles)
      (let ((number (gethash (plist-get article :id) numbers)))
        (unless number (setq number next next (1+ next)))
        (setf (plist-get article :number) number)))
    (let ((previous nngithub--state))
      (setq nngithub--state (copy-tree nngithub--state))
      (setcdr (assoc group (plist-get nngithub--state :groups))
              (list :next next :articles articles :roots stamps :synced t))
      (condition-case err (nngithub--save)
        (error (setq nngithub--state previous) (signal (car err) (cdr err)))))
    t))

(defmacro nngithub--with-report (&rest body)
  "Run BODY, reporting backend errors to Gnus."
  (declare (indent 0) (debug t))
  `(condition-case err (progn ,@body)
     (error (nnheader-report 'nngithub "%s" (error-message-string err)) nil)))

(defun nngithub--groups ()
  "Return discovered and locally registered groups on the current server."
  (delete-dups
   (append (mapcar #'car (plist-get nngithub--state :groups))
           (copy-sequence (plist-get nngithub--state :catalog)))))

(defun nngithub--discover ()
  "Discover groups from the authenticated user's watched repositories."
  (let (groups)
    (dolist (repo (nngithub--connection
                   "target: viewer" "watching" nil
                   "nameWithOwner hasIssuesEnabled hasDiscussionsEnabled"))
      (let ((name (alist-get 'nameWithOwner repo)))
        (push (concat name ".pulls") groups)
        (when (alist-get 'hasIssuesEnabled repo)
          (push (concat name ".issues") groups))
        (when (alist-get 'hasDiscussionsEnabled repo)
          (push (concat name ".discussions") groups))))
    (setf (plist-get nngithub--state :catalog) (nreverse groups))
    (nngithub--save)))

(defun nngithub--subscribed-groups ()
  "Return Gnus subscriptions belonging to the currently selected server."
  (let ((method (list 'nngithub (nnoo-current-server 'nngithub))))
    (cl-loop for info in (cdr gnus-newsrc-alist)
             for name = (gnus-info-group info)
             when (and (<= (gnus-info-level info) gnus-level-subscribed)
                       (gnus-server-equal
                        method (gnus-find-method-for-group name)))
             collect (gnus-group-real-name name))))

(defun nngithub--insert-active (groups)
  "Insert active information for GROUPS without creating or fetching them."
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (group groups)
      (let ((state (cdr (assoc group (plist-get nngithub--state :groups)))))
        (insert (format "%s %d 1 y\n" group
                        (1- (or (plist-get state :next) 1)))))))
  'active)

(deffoo nngithub-request-list (&optional server)
  (nngithub--with-report
    (nngithub--select server)
    (nngithub--discover)
    (nngithub--insert-active (nngithub--groups))
    t))

(deffoo nngithub-retrieve-groups (groups &optional server)
  (nngithub--with-report
    (nngithub--select server)
    (nngithub--insert-active groups)))

(deffoo nngithub-request-group (group &optional server dont-check _info)
  (nngithub--with-report
    (nngithub--select server)
    (unless (or dont-check (plist-get (nngithub--group group) :synced))
      (nngithub--sync group))
    (let* ((state (nngithub--group group)) (articles (plist-get state :articles)))
      (nnheader-insert "211 %d %d %d %s\n" (length articles)
                       (if articles (apply #'min (mapcar (lambda (a) (plist-get a :number)) articles)) 1)
                       (1- (plist-get state :next)) group))
    t))

(deffoo nngithub-request-scan (&optional group server)
  (nngithub--with-report
    (nngithub--select server)
    (let (failures)
      (dolist (name (if group (list group) (nngithub--subscribed-groups)))
        (condition-case err (nngithub--sync name)
          (error (push (format "%s: %s" name (error-message-string err))
                       failures))))
      (if failures
          (progn
            (nnheader-report 'nngithub "%s"
                             (string-join (nreverse failures) "; "))
            nil)
        t))))

(deffoo nngithub-request-create-group (group &optional server _args)
  (nngithub--with-report
    (nngithub--select server)
    (pcase-let* ((`(,owner ,repo ,kind) (nngithub--parse-group group))
                 (repository
                  (alist-get
                   'repository
                   (nngithub--api
                    (concat "query($owner: String!, $name: String!) { "
                            "repository(owner: $owner, name: $name) { "
                            "id hasIssuesEnabled hasDiscussionsEnabled } }")
                    `((owner . ,owner) (name . ,repo))))))
      (unless repository (error "Repository unavailable: %s/%s" owner repo))
      (when (or (and (equal kind "issues")
                     (not (alist-get 'hasIssuesEnabled repository)))
                (and (equal kind "discussions")
                     (not (alist-get 'hasDiscussionsEnabled repository))))
        (error "%s are disabled for %s/%s" kind owner repo)))
    (nngithub--group group)
    (nngithub--save)
    t))

(deffoo nngithub-close-group (_group &optional _server) t)
(deffoo nngithub-request-type (_group &optional _article) 'news)

(defun nngithub--find (article group)
  "Find ARTICLE number or Message-ID in GROUP."
  (cl-find article (plist-get (nngithub--group group) :articles)
           :key (lambda (a) (plist-get a (if (numberp article) :number :message-id)))
           :test #'equal))

(defun nngithub--clean (value)
  "Make VALUE safe for a single header line."
  (replace-regexp-in-string "[\r\n\t]+" " " (or value "")))

(defun nngithub--date (article)
  "RFC date for ARTICLE."
  (format-time-string "%a, %d %b %Y %T %z"
                      (date-to-time (plist-get article :date)) t))

(defun nngithub--from (article)
  "Synthetic author address for ARTICLE, deliberately not deliverable."
  (format "%s <author@nngithub.invalid>" (nngithub--clean (plist-get article :author))))

(deffoo nngithub-retrieve-headers (articles &optional group server _fetch-old)
  (nngithub--with-report
    (nngithub--select server)
    (when (and (consp articles) (numberp (cdr articles)))
      (setq articles (number-sequence (car articles) (cdr articles))))
    (let ((index (make-hash-table :test #'equal)))
      (dolist (article (plist-get (nngithub--group group) :articles))
        (puthash (plist-get article :number) article index)
        (puthash (plist-get article :message-id) article index))
      (with-current-buffer nntp-server-buffer
        (erase-buffer)
        (dolist (number articles)
          (when-let* ((article (gethash number index)))
            (nnheader-insert-nov
             (make-full-mail-header
              (plist-get article :number)
              (nngithub--clean (plist-get article :subject))
              (nngithub--from article) (nngithub--date article)
              (plist-get article :message-id) (plist-get article :references)
              (string-bytes (plist-get article :body))
              (length (split-string (plist-get article :body) "\n")) nil nil))))))
    'nov))

(deffoo nngithub-request-article (number &optional group server buffer)
  (nngithub--with-report
    (nngithub--select server)
    (when-let* ((article (nngithub--find number group)))
      (with-current-buffer (or buffer nntp-server-buffer)
        (erase-buffer)
        (insert "From: " (nngithub--from article)
                "\nSubject: " (nngithub--clean (plist-get article :subject))
                "\nDate: " (nngithub--date article)
                "\nMessage-ID: " (plist-get article :message-id)
                "\nReply-To: " (substring (plist-get article :message-id) 1 -1)
                "\nNewsgroups: " group
                "\nArchived-At: <" (nngithub--clean (plist-get article :url)) ">"
                "\nMIME-Version: 1.0\nContent-Type: text/plain; charset=utf-8"
                "\nContent-Transfer-Encoding: 8bit\n")
        (when (plist-get article :references)
          (insert "References: " (plist-get article :references) "\n"))
        (insert "\n" (plist-get article :body) "\n\n"
                (or (plist-get article :url) "") "\n"))
      (cons group (plist-get article :number)))))

(defun nngithub--mutation (name type input result)
  "Run mutation NAME with input TYPE and INPUT, selecting RESULT."
  (let ((value (alist-get (intern name)
                          (nngithub--api
                           (format "mutation($input: %s!) { %s(input: $input) { %s } }"
                                   type name (if (equal result "commentEdge")
                                                 "commentEdge { node { id url } }"
                                               (format "%s { id url }" result)))
                           `((input . ,input))))))
    (or (if (equal result "commentEdge")
            (nngithub--get value 'commentEdge 'node)
          (alist-get (intern result) value))
        (error "GitHub mutation returned no result; inspect GitHub before resending"))))

(defun nngithub--reply (article body group)
  "Post BODY as a reply to ARTICLE in GROUP."
  (cond
   ((plist-get article :review)
    (nngithub--mutation "addPullRequestReviewThreadReply" "AddPullRequestReviewThreadReplyInput"
                        `((pullRequestReviewThreadId . ,(plist-get article :review))
                          (body . ,body)) "comment"))
   ((equal (nth 2 (nngithub--parse-group group)) "discussions")
    (nngithub--mutation "addDiscussionComment" "AddDiscussionCommentInput"
                        `((discussionId . ,(plist-get article :root)) (body . ,body)
                          ,@(when (plist-get article :reply)
                              `((replyToId . ,(plist-get article :reply))))) "comment"))
   (t (nngithub--mutation "addComment" "AddCommentInput"
                          `((subjectId . ,(plist-get article :root)) (body . ,body))
                          "commentEdge"))))

(defun nngithub--body ()
  "Return the raw Markdown body, refusing MIME that would be lost."
  (save-excursion
    (message-goto-body)
    (let ((body (buffer-substring-no-properties (point) (point-max)))
          (type (message-fetch-field "Content-Type")))
      (when (or (string-match-p "<#\\(?:part\\|multipart\\|secure\\)\\b" body)
                (and type (not (string-match-p "\\`text/plain\\b" type)))
                (let ((encoding (message-fetch-field "Content-Transfer-Encoding")))
                  (and encoding (not (member (downcase encoding) '("7bit" "8bit" "binary"))))))
        (user-error "GitHub accepts plain Markdown only; remove MIME/attachments/encryption"))
      (when (string-empty-p (string-trim body)) (user-error "Empty GitHub message"))
      body)))

(defun nngithub--new (group body)
  "Create a conversation in GROUP using current message headers and BODY."
  (pcase-let* ((`(,owner ,repo ,kind) (nngithub--parse-group group))
               (repository (alist-get 'repository
                                      (nngithub--api
                                       "query($owner: String!, $name: String!) { repository(owner: $owner, name: $name) { id } }"
                                       `((owner . ,owner) (name . ,repo)))))
               (title (message-fetch-field "Subject"))
               (input `((repositoryId . ,(alist-get 'id repository))
                        (title . ,title) (body . ,body))))
    (unless (and title (not (string-empty-p (string-trim title))))
      (user-error "A new conversation requires a Subject"))
    (pcase kind
      ("issues" (nngithub--mutation "createIssue" "CreateIssueInput" input "issue"))
      ("discussions"
       (let* ((category (message-fetch-field "X-GitHub-Category"))
              (categories (nngithub--children (alist-get 'id repository) "Repository"
                                              "discussionCategories" "id name"))
              (match (cl-find-if (lambda (c) (or (equal category (alist-get 'id c))
                                                 (equal category (alist-get 'name c)))) categories)))
         (unless match (user-error "Set X-GitHub-Category to one of: %s"
                                   (mapconcat (lambda (c) (alist-get 'name c)) categories ", ")))
         (nngithub--mutation "createDiscussion" "CreateDiscussionInput"
                             (cons (cons 'categoryId (alist-get 'id match)) input) "discussion")))
      ("pulls"
       (let ((base (message-fetch-field "X-GitHub-Base"))
             (head (message-fetch-field "X-GitHub-Head")))
         (unless (and base head (not (string-empty-p base)) (not (string-empty-p head)))
           (user-error "Set X-GitHub-Base and X-GitHub-Head to pushed branches"))
         (nngithub--mutation "createPullRequest" "CreatePullRequestInput"
                             (append `((baseRefName . ,base) (headRefName . ,head)) input) "pullRequest"))))))

(defun nngithub--post (group)
  "Post the current message to GROUP, resolving References from the cache."
  (let* ((refs (concat (or (message-fetch-field "References")
                           (message-make-references)) " "
                           (message-fetch-field "In-Reply-To")))
         (ids (let ((start 0) result)
                (while (string-match "<[^<>[:space:]]+>" refs start)
                  (push (match-string 0 refs) result) (setq start (match-end 0)))
                result))
         (parent (and ids (nngithub--find (car ids) group)))
         (body (nngithub--body)))
    (when (and ids (not parent))
      (user-error "Reply target missing from cache; refresh the group before replying"))
    (prog1 (if parent (nngithub--reply parent body group) (nngithub--new group body))
      ;; Do not fail a successful write because a subsequent cache refresh fails.
      (let ((state (nngithub--group group)))
        (setf (plist-get state :synced) nil)))))

(deffoo nngithub-request-post (&optional server)
  (nngithub--with-report
    (nngithub--select server)
    (let ((group (message-fetch-field "Newsgroups")))
      (when (or (null group) (string-match-p "," group))
        (user-error "Post to exactly one GitHub group"))
      (nngithub--post (gnus-group-real-name group)))
    t))

(defun nngithub--reply-target ()
  "Find a synthetic reply address in outgoing recipient fields."
  (let ((text (mapconcat (lambda (field) (or (message-fetch-field field) ""))
                         '("To" "Cc" "Bcc") " ")))
    (when (string-match "\\([[:xdigit:]]\\{64\\}\\)@nngithub\\.invalid" text)
      (concat "<" (match-string 0 text) ">"))))

(defun nngithub--locate (id)
  "Resolve Message-ID ID across configured nngithub servers."
  (let ((methods (delete-dups
                  (append (list (list 'nngithub (nnoo-current-server 'nngithub)))
                          (cl-remove-if-not (lambda (m) (eq (car-safe m) 'nngithub))
                                            (append (list gnus-select-method)
                                                    gnus-secondary-select-methods
                                                    (mapcar #'cdr gnus-server-alist))))))
        found)
    (while (and methods (not found))
      (let ((method (pop methods)))
        (when (cadr method)
          (nngithub-open-server (cadr method) (cddr method))
          (nngithub--load)
          (dolist (entry (plist-get nngithub--state :groups))
            (when-let* ((article (nngithub--find id (car entry))))
              (setq found (list (car entry) article)))))))
    (or found (user-error "GitHub reply target missing; reopen its group and retry"))))

(defun nngithub--send-reply (&optional _arg)
  "Send the current Message buffer to its synthetic GitHub recipient."
  (let* ((id (or (nngithub--reply-target) (user-error "Missing GitHub recipient")))
         (addresses (mail-header-parse-addresses
                     (mapconcat (lambda (f) (or (message-fetch-field f) ""))
                                '("To" "Cc" "Bcc") ","))))
    (unless (and addresses
                 (cl-every (lambda (a) (equal (car a) (substring id 1 -1))) addresses)
                 (not (message-fetch-field "Newsgroups")))
      (user-error "GitHub replies must have one GitHub recipient and no Cc/Bcc/newsgroups"))
    (pcase-let ((`(,group ,article) (nngithub--locate id)))
      (nngithub--reply article (nngithub--body) group)
      (let ((state (nngithub--group group)))
        (setf (plist-get state :synced) nil)))
    (cl-pushnew 'nngithub message-sent-message-via)
    t))

(defun nngithub--prepare-send ()
  "Route synthetic GitHub recipients through the API, including saved drafts."
  (when-let* ((groups (message-fetch-field "Newsgroups")))
    (when (string-match-p "\\(?:\\`\\|[, ]\\)[[:alnum:]_-]+/[[:alnum:]_.-]+\\.\\(?:issues\\|pulls\\|discussions\\)\\(?:\\'\\|[, ]\\)" groups)
      (let ((method (if (functionp message-post-method)
                        (funcall message-post-method nil) message-post-method)))
        (when (eq (car-safe method) 'nngithub)
          (setq-local message-post-method method)
          (setq-local message-send-method-alist '((nngithub always nngithub--send-post)))))))
  (when (cl-some (lambda (field)
                   (string-match-p "@nngithub\\.invalid" (or (message-fetch-field field) "")))
                 '("To" "Cc" "Bcc"))
    (unless (nngithub--reply-target)
      (user-error "Synthetic GitHub author address is not deliverable; use Reply-To"))
    (setq-local message-send-method-alist '((nngithub always nngithub--send-reply)))))

(defun nngithub--setup-reply ()
  "Recognize local API addresses when composing or restoring a draft."
  (when (nngithub--reply-target)
    ;; Our sender validates the exact sole recipient; .invalid is intentional.
    (when message-syntax-checks
      (setq-local message-syntax-checks
                  (cons '(bogus-recipient . disabled)
                        (when (listp message-syntax-checks) message-syntax-checks))))
    (setq-local message-send-method-alist '((nngithub always nngithub--send-reply)))))

(defun nngithub--send-post (&optional _arg)
  "Send a raw Markdown news composition without MIME or SMTP conversion."
  (when (cl-some #'message-fetch-field '("To" "Cc" "Bcc"))
    (user-error "GitHub posts cannot also send mail; remove To/Cc/Bcc"))
  (nngithub-open-server (cadr message-post-method) (cddr message-post-method))
  (unless (nngithub-request-post (cadr message-post-method))
    (error "%s" (nngithub-status-message)))
  (cl-pushnew 'nngithub message-sent-message-via)
  t)

(defun nngithub--current-group ()
  "Select the current Gnus group's backend and return its real name."
  (let* ((name (if (derived-mode-p 'gnus-group-mode)
                   (gnus-group-group-name) gnus-newsgroup-name))
         (method (and name (gnus-find-method-for-group name))))
    (unless (eq (car-safe method) 'nngithub) (user-error "Not a GitHub group"))
    (nngithub-open-server (cadr method) (cddr method))
    (nngithub--load)
    (gnus-group-real-name name)))

(defun nngithub-rescan ()
  "Refresh all conversations in the current group, including unchanged ones."
  (interactive)
  (nngithub--sync (nngithub--current-group) t)
  (message "GitHub group refreshed; use g to update the Gnus display"))

(defun nngithub-browse ()
  "Open the current GitHub article in a browser."
  (interactive)
  (with-current-buffer (if (derived-mode-p 'gnus-article-mode)
                           (or (get-buffer gnus-summary-buffer)
                               (user-error "No live Gnus summary buffer"))
                         (current-buffer))
    (let* ((group (nngithub--current-group))
           (article (nngithub--find (gnus-summary-article-number) group)))
      (unless article (user-error "No GitHub article at point"))
      (browse-url (plist-get article :url)))))

(defun nngithub-compose ()
  "Compose a new conversation in the current Gnus GitHub group."
  (interactive)
  (let* ((group (nngithub--current-group))
         (kind (nth 2 (nngithub--parse-group group)))
         (headers (pcase kind
                    ("discussions" (list (cons "X-GitHub-Category" (read-string "Discussion category: "))))
                    ("pulls" (list (cons "X-GitHub-Base" (read-string "Base branch: "))
                                   (cons "X-GitHub-Head" (read-string "Pushed head branch: ")))))))
    (gnus-post-news 'post (gnus-group-prefixed-name
                           group (list 'nngithub (nnoo-current-server 'nngithub))))
    (dolist (header headers) (message-add-header (format "%s: %s\n" (car header) (cdr header))))))

(add-hook 'message-send-hook #'nngithub--prepare-send)
(add-hook 'message-setup-hook #'nngithub--setup-reply)
(add-hook 'gnus-draft-setup-hook #'nngithub--setup-reply)
(unless (assoc "nngithub" gnus-valid-select-methods)
  (gnus-declare-backend "nngithub" 'post 'address))

(provide 'nngithub)
;;; nngithub.el ends here
