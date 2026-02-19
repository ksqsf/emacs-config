;;; -*- lexical-binding: t; -*-

(with-eval-after-load 'diff-mode
  (keymap-unset diff-mode-map "M-o"))

(with-eval-after-load 'ediff
  (setq ediff-split-window-function #'split-window-horizontally))

(use-package magit
  :defer t
  :iload (magit-base magit-git magit-mode magit-process magit-status magit-submodule magit)
  :bind (("C-x g" . magit-status))
  :custom
  (magit-clone-set-remote.pushDefault t)
  (magit-clone-default-directory (expand-file-name (expand-file-name "src/Clone/" (getenv "HOME"))))
  (magit-refresh-status-buffer nil)
  (magit-display-buffer 'display-buffer) ;; I've done my own customization
  :bind (("C-c g" . magit-file-dispatch))
  :hook ((magit-status-mode magit-diff-mode) . buffer-disable-undo)
  :config
  (setq magit-wip-mode +1)
  (when k|mac
    (setq magit-git-executable "/usr/local/bin/git"))
  (setq magit-tramp-pipe-stty-settings 'pty))

(use-package forge
  :after magit
  :config
  (setq forge-owned-accounts '(("ksqsf" . (remote-name "personal")))))

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
  (let* ((remote (or (magit-get-remote)
                     (magit-get-upstream-remote)
                     (magit-get-remote "main")
                     (magit-get-remote "master")
                     (error "Unknown remote!")))
         (remote-url (magit-git-str "remote" "get-url" remote))
         (commit (magit-rev-parse "--short" "HEAD"))
         (user/repo (github-parse-remote-url remote-url))
         (relative-path (file-relative-name buffer-file-name (vc-git-root buffer-file-name)))
         (locator (if (use-region-p)
                      (format "L%d,L%d" (line-number-at-pos (use-region-beginning)) (line-number-at-pos (use-region-end)))
                    (format "L%d" (line-number-at-pos))))
         (url (format "https://github.com/%s/%s/blob/%s/%s#%s" (car user/repo) (cdr user/repo) commit relative-path locator)))
    (kill-new url)
    (message "%s" url)))

;; Inspired by https://twitter.com/samcraigjohnson/status/1747412804112703734
(defun open-github-pullreq ()
  "Visit the github pr creation link."
  (interactive)
  (magit-process-buffer)
  (goto-char (point-max))
  (re-search-backward "https://github.com/[^ ]+pull[^ ]")
  (let ((url (buffer-substring (point) (pos-eol))))
    (browse-url (s-trim url))))

(defun open-gitlab-mergereq ()
  (interactive)
  (magit-process-buffer)
  (goto-char (point-max))
  (re-search-backward "https://[^ ]+/merge_requests/[^ ]+")
  (let ((url (buffer-substring (point) (pos-eol))))
    (browse-url (s-trim url))))

(provide 'prelude-git)
