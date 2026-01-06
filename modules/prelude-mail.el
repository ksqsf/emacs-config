;; -*- lexical-binding: t; -*-

;; Be sure to customize:
;; * user-full-name       will be displayed on all emails you send
;; * user-mail-address    a GMail address

(use-package gnus
  :commands (gnus)
  :ensure nil
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
                                   (nntp "nntp.lore.kernel.org")
                                   (nnimap "GMail"
                                           (nnimap-inbox "INBOX")
                                           (nnimap-address "imap.gmail.com")
                                           (nnimap-server-port "imaps")
                                           (nnimap-stream ssl)
                                           (nnimap-expunge 'nerver))))
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
  (setq gnus-visible-headers (rx line-start (or "From"
                                           "Subject"
                                           "Mail-Followup-To"
                                           "Date"
                                           "To"
                                           "Cc"
                                           "Newsgroups"
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
  (setq gnus-cacheable-groups "^\\(nntp\\|nnimap\\)"))

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
  (setq send-mail-function #'smtpmail-send-it)
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-user user-mail-address)
  (setq smtpmail-smtp-service 587))

(provide 'prelude-mail)
