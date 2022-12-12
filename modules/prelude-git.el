;;; -*- lexical-binding: t; -*-

(use-package magit
  :defer t
  :custom
  (magit-clone-set-remote.pushDefault t)
  (magit-clone-default-directory (expand-file-name (expand-file-name "src/Clone/" (getenv "HOME"))))
  :bind (("C-c g" . magit-file-dispatch)))

(use-package forge
  :after magit
  :defer t
  :config
  (setq forge-owned-accounts '(("ksqsf" . (remote-name "personal")))))

;; (require 'org-protocol)

;; (setq org-protocol-protocol-alist
;;       '(("git-clone"
;;          :protocol "git-clone"
;;          :function org-protocol-git-clone)))

;; (defvar org-protocol-git-clone-directory 
;;   "Default directory for `org-protocol-git-clone`.")

;; (defun org-protocol-git-clone (info)
;;   "Process an org-protocol://git-clone style url with INFO."
;;   (let ((url (plist-get info :url)))
;;     (message "%s" url)
;;     (vc-git-clone url org-protocol-git-clone-directory nil))
;;   nil)

(defun github-parse-remote-url (remote)
  "Parse a git remote hosted on github to a (user,repo) pair.

This function returns nil if it cannot parse REMOTE."
  (let ((ssh-regexp "git@github\\.com:\\(.*\\)/\\(.*\\)\\.git")
        (https-regexp "https://github.com/\\(.*\\)/\\(.*\\)\\.git"))
    (let ((maybe-ssh (string-match ssh-regexp remote)))
      (if maybe-ssh
          (cons (match-string-no-properties 1 remote) (match-string-no-properties 2 remote))
        (let ((maybe-https (string-match https-regexp remote)))
          (if maybe-https
              (cons (match-string-no-properties 1 remote) (match-string-no-properties 2 remote))
            nil))))))

(defun github-copy-reference-url-at-point ()
  "Copy a link to the current line on the GitHub Web interface."
  (interactive)
  (require 'magit)
  (save-buffer)
  (let* ((remote (magit-get-remote))
         (remote-url (magit-git-str "remote" "get-url" remote))
         (commit (magit-rev-parse "--short" "HEAD"))
         (user/repo (github-parse-remote-url remote-url))
         (relative-path (file-relative-name buffer-file-name (vc-git-root buffer-file-name)))
         (url (format "https://github.com/%s/%s/blob/%s/%s#L%d" (car user/repo) (cdr user/repo) commit relative-path (line-number-at-pos))))
    (kill-new url)
    (message "%s" url)))

(provide 'prelude-git)
