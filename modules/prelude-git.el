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

(provide 'prelude-git)
