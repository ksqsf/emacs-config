;; -*- lexical-binding: t; -*-

;; Be sure to customize:
;; * user-full-name       will be displayed on all emails you send
;; * user-mail-address    the primary, default mail address

;; For incoming news/mails, consider customizing:
;; * gnus-secondary-select-methods

;; For sending mails, consider customizing:
;; * message-alternative-emails
;; * smtpmail-servers-requiring-authorization

(use-package gnus
  :commands (gnus)
  :ensure nil
  :hook (((gnus-group-mode
           gnus-summary-mode
           gnus-article-mode)
          . hl-line-mode))
  :bind (:map gnus-summary-mode-map
              ("C-c C-c" . +gnus-summary-copy-permalink)
              :map gnus-article-mode-map
              ("C-c C-c" . +gnus-article-copy-permalink))
  :config
  (setq gnus-use-cache t)
  (setq gnus-use-scoring nil)
  (setq gnus-suppress-duplicates t)
  (setq gnus-novice-user nil)
  (setq gnus-expert-user t)
  (setq gnus-interactive-exit 'quiet)
  (setq gnus-inhibit-startup-message t)
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods '((nntp "news.gmane.io")
                                        (nntp "nntp.lore.kernel.org")))
  ;;; Startup functions
  (setq gnus-save-killed-list nil)
  (setq gnus-check-new-newsgroups 'ask-server)
  ;; No other newsreader is used.
  (setq gnus-save-newsrc-file nil)
  (setq gnus-read-newsrc-file nil)
  (setq gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies)
  ;; Emacs 28 introduces a unified query lang
  (setq gnus-search-use-parsed-queries t)
  ;;; Article mode for Gnus
  (setq gnus-visible-headers (rx line-start
                                 (or "From"
                                     "Subject"
                                     "Mail-Followup-To"
                                     "Date"
                                     "To"
                                     "Cc"
                                     "Newsgroups"
                                     "Message-ID"
                                     "User-Agent"
                                     "X-Mailer"
                                     "X-Newsreader")
                                 ":"))
  (setq gnus-article-sort-functions '((not gnus-article-sort-by-number)
                                 (not gnus-article-sort-by-date)))
  (setq gnus-article-browse-delete-temp t)
  ;; Display more MINE stuff
  (setq gnus-mime-display-multipart-related-as-mixed t)
  ;;; Asynchronous support for Gnus
  (setq gnus-asynchronous t)
  (setq gnus-use-header-prefetch t)
  ;;; Cache interface for Gnus
  (setq gnus-cache-enter-articles '(ticked dormant unread))
  (setq gnus-cache-remove-articles '(read))
  (setq gnus-cacheable-groups "^\\(nntp\\|nnimap\\)")
  ;; Display smileys
  (setq smiley-style 'emoji)
  (setq gnus-treat-display-smileys t))

;; Group mode commands for Gnus
(use-package gnus-group
  :ensure nil
  :defer t
  :hook (gnus-group-mode . gnus-topic-mode)
  :config
  (require 'gnus-topic)
  ;;          indentation ------------.
  ;;  #      process mark ----------. |
  ;;                level --------. | |
  ;;           subscribed ------. | | |
  ;;  %          new mail ----. | | | |
  ;;  *   marked articles --. | | | | |
  ;;                        | | | | | |  Ticked    New     Unread  open-status Group
  (setq gnus-group-line-format "%M%m%S%L%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%) %d\n")
  (setq gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet)))

;; Summary mode commands for Gnus
(use-package gnus-sum
  :ensure nil
  :defer t
  :hook (gnus-select-group . gnus-group-set-timestamp)
  :config
  ;; Pretty marks
  (setq gnus-sum-thread-tree-root            "┌ ")
  (setq gnus-sum-thread-tree-false-root      "◌ ")
  (setq gnus-sum-thread-tree-single-indent   "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-indent          "  ")
  (setq gnus-sum-thread-tree-leaf-with-other "├─►")
  (setq gnus-sum-thread-tree-single-leaf     "╰─►")
  (setq gnus-summary-line-format "%U%R %3d %[%-23,23f%] %B %s\n")
  ;; Loose threads
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  ;; Filling in threads
  ;; 2 old articles are enough for memory
  (setq gnus-fetch-old-headers 2)
  (setq gnus-fetch-old-ephemeral-headers 2)
  (setq gnus-build-sparse-threads 'some)
  ;; More threading
  (setq gnus-show-threads t)
  (setq gnus-thread-indent-level 2)
  (setq gnus-thread-hide-subtree nil)
  ;; Sorting
  (setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (setq gnus-subthread-sort-functions '(gnus-thread-sort-by-date))
  ;; Viewing
  (setq gnus-view-pseudos 'automatic)
  (setq gnus-view-pseudos-separately t)
  (setq gnus-view-pseudo-asynchronously t)
  ;; No auto select
  (setq gnus-auto-select-first nil)
  (setq gnus-auto-select-next nil)
  (setq gnus-paging-select-next nil))

(use-package message
  :ensure nil
  :defer t
  :hook (message-mode . auto-fill-mode)
  :config
  (setq message-kill-buffer-on-exit t)
  (setq message-signature user-full-name)
  (setq message-mail-alias-type 'ecomplete)
  (setq message-send-mail-function #'message-use-send-mail-function)
  (setq send-mail-function #'+smtp-send-it)
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-user user-mail-address)
  (setq smtpmail-smtp-service 587))

;;
;; Smarter SMTP settings
;;
(defvar +smtp-accounts nil
  "List of SMTP accounts.

An SMTP account is specified as a list:
(FROM SMTP-SERVER SMTP-PORT SMTP-STREAM-TYPE SMTP-USER)

The password (if needed) is determined from authinfo with AUTH-LOGIN.

FROM is the 'From' field of the message.  SMTP-SERVER, SMTP-PORT,
SMTP-STREAM-TYPE, and SMTP-USER correspond to 'smtpmail-smtp-server',
'smtpmail-smtp-service', 'smtpmail-stream-type', and
'smtpmail-smtp-user'.

SMTP-USER is optional, and defaults to FROM.

Consider customizing 'message-alternative-emails' if you use multiple
accounts.")

(defun +smtp-send-it ()
  "Send an E-mail with the SMTP user determined from From."
  (let* ((from (message-fetch-field "From"))
         (address (and from (cadr (mail-extract-address-components from))))
         (account (assoc address +smtp-accounts)))
    (unless account
      (user-error "No SMTP account for %s" address))
    (let ((smtpmail-smtp-server (nth 1 account))
          (smtpmail-smtp-service (nth 2 account))
          (smtpmail-stream-type (nth 3 account))
          (smtpmail-smtp-user (or (nth 4 account) address)))
      (smtpmail-send-it))))

(defvar +gnus-permalink-templates
  '(("gmane.emacs.bugs" "https://yhetil.org/emacs-bugs/%M")
    ("gmane.emacs.devel" "https://yhetil.org/emacs-devel/%M")
    ("gmane.emacs.help" "https://yhetil.org/emacs-user/%M")
    ("gmane.emacs.tangents" "https://yhetil.org/emacs-tangents/%M")
    ("gmane.emacs.orgmode" "https://yhetil.org/orgmode/%M")
    ("gmane.lisp.guile.bugs" "https://yhetil.org/guile-bugs/%M")
    ("gmane.lisp.guile.devel" "https://yhetil.org/guile-devel/%M")
    ("gmane.lisp.guile.user" "https://yhetil.org/guile-user/%M")
    ("gmane.comp.gnu.guix.bugs" "https://yhetil.org/guix-bugs/%M")
    ("gmane.mail.public-inbox.general" "https://yhetil.org/meta/%M")
    ("gmane.mail.notmuch.general" "https://yhetil.org/notmuch/%M")
    ("gmane.comp.gnu.guix.devel" "https://yhetil.org/guix-devel/%M")
    ("gmane.comp.gnu.guix.patches" "https://yhetil.org/guix-patches/%M")
    ("gmane.comp.gnu.guix.help" "https://yhetil.org/guix-user/%M")
    ("org.kernel.vger.linux-kernel" "https://lore.kernel.org/lkml/%M")
    ("dev.linux.lists.lkmm" "https://lore.kernel.org/lkmm/%M")
    ("org.kernel.vger.rcu" "https://lore.kernel.org/rcu/%M")
    ("gmane.comp.version-control.git" "https://lore.kernel.org/git/%M")
    ("org.kernel.vger.git" "https://lore.kernel.org/git/%M"))
  "Templates for copying the permalink of an article.

%M   Message-ID.")

(defun +gnus-copy-permalink (group message-id)
  "Copy the archive permalink for MESSAGE-ID in GROUP."
  (let ((template (cadr (assoc (gnus-group-real-name group) +gnus-permalink-templates)))
        (id (and (stringp message-id) (string-trim message-id))))
    (unless template
      (user-error "Don't know the permalink format for %s" group))
    (when (and id
               (string-prefix-p "<" id)
               (string-suffix-p ">" id))
      (setq id (substring id 1 -1)))
    (when (or (null id) (string-empty-p id))
      (user-error "This article has no Message-ID"))
    (let ((permalink
           (replace-regexp-in-string
            "%M" (url-hexify-string id) template t t)))
      (kill-new permalink)
      (message "Copied: %s" permalink))))

(defun +gnus-summary-copy-permalink ()
  "Copy the current Gnus summary article's archive permalink."
  (interactive)
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "Not in a Gnus summary buffer"))
  (let ((header (gnus-summary-article-header)))
    (+gnus-copy-permalink gnus-newsgroup-name
                          (and header (mail-header-id header)))))

(defun +gnus-article-copy-permalink ()
  "Copy the displayed Gnus article's archive permalink."
  (interactive)
  (unless (derived-mode-p 'gnus-article-mode)
    (user-error "Not in a Gnus article buffer"))
  (let ((summary (get-buffer gnus-summary-buffer))
        (message-id
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (mail-fetch-field "message-id")))))
    (unless (buffer-live-p summary)
      (user-error "No live Gnus summary buffer"))
    (+gnus-copy-permalink
     (buffer-local-value 'gnus-newsgroup-name summary)
     message-id)))

(provide 'prelude-mail)
