;;; -*- lexical-binding: t; -*-

(use-package magit
  :defer t
  :bind (("C-c g" . magit-file-dispatch)))

(use-package forge
  :after magit
  :defer t)

;; Streamline the process of contributing to GitHub repositories.
;; Doesn't seem to work.
;; Will write one using the Native Messaging API soon(TM).
;; https://developer.chrome.com/docs/apps/nativeMessaging/
;;
;; (require 'org-protocol)
;; (add-to-list 'org-protocol-protocol-alist
;;              '(("git-clone"
;;                 :protocol "git-clone"
;;                 :function git-clone-protocol-handler)))
;; (defun git-clone-protocol-handler (url)
;;   (message "OK!!")
;;   (magit-clone-regular url "~/src/Clone/" nil))

(provide 'prelude-git)
