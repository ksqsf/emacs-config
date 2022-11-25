;; -*- lexical-binding: t; -*-

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

;; Add a "Remove" button to the advice list
;; Credit: https://emacs-china.org/t/advice/7566
(add-hook 'help-mode-hook 'cursor-sensor-mode)

(defun function-advices (function)
  "Return FUNCTION's advices."
  (let ((function-def (advice--symbol-function function))
        (ad-functions '()))
    (while (advice--p function-def)
      (setq ad-functions (append `(,(advice--car function-def)) ad-functions))
      (setq function-def (advice--cdr function-def)))
    ad-functions))

(defconst +prelude--advice-regex+
  (if (version<= "27.1" emacs-version)
      "^\\(?:This function has \\)?:[-a-z]+ advice: [‘'`]\\(.+\\)[’'']\\.?$"
    "^:[-a-z]+ advice: [‘'`]\\(.+\\)[’'']$"))

(define-advice describe-function-1 (:after (function) advice-remove-button)
  "Add a button to remove advice."
  (when (get-buffer "*Help*")
    (with-current-buffer "*Help*"
      (save-excursion
        (goto-char (point-min))
        (let ((ad-index 0)
              (ad-list (reverse (function-advices function))))
          (while (re-search-forward +prelude--advice-regex+ nil t)
            (let* ((name (string-trim (match-string 1) "'" "'"))
                   (advice (or (intern-soft name) (nth ad-index ad-list))))
              (when (and advice (functionp advice))
                (let ((inhibit-read-only t))
                  (insert " » ")
                  (insert-text-button
                   "Remove"
                   'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                   'help-echo (format "%s" advice)
                   'action
                   ;; In case lexical-binding is off
                   `(lambda (_)
                      (when (yes-or-no-p (format "Remove %s ? " ',advice))
                        (message "Removing %s of advice from %s" ',function ',advice)
                        (advice-remove ',function ',advice)
                        (revert-buffer nil t)))
                   'follow-link t))))
            (setq ad-index (1+ ad-index))))))))

(defun Info-get-online-url ()
  (let* ((file (file-name-sans-extension
	        (file-name-nondirectory Info-current-file)))
         (node Info-current-node)
         (node (string-replace " " "-" node))
         (node (if (string= node "Top") "index"
                 node))
         (url (format
               "https://www.gnu.org/software/emacs/manual/html_node/%s/%s.html"
               file node)))
    url))

(defun Info-copy-online-url (&optional nocopy)
  "Put the online url of the current Info node into the kill ring.

This command is only meaningful for the official manuals, and it
does not work in TOC nodes."
  (interactive)
  (let ((url (Info-get-online-url)))
    (kill-new url)
    (message "%s" url)))

(defun Info-copy-markdown-link ()
  "Put a markdown link to the online manual.

The link is obtained as if it was returned from
`Info-copy-online-url', and the label
`Info-copy-current-node-name'."
  (interactive)
  (let* ((file (file-name-sans-extension
	        (file-name-nondirectory Info-current-file)))
         (node Info-current-node)
         (url (Info-get-online-url))
         (md-link (format "[(%s) %s](%s)" file node url)))
    (kill-new md-link)
    (message "%s" md-link)))

(use-package info
  :ensure nil
  :bind (:map
         Info-mode-map
         ("C" . Info-copy-online-url)
         ("C-c C-c" . Info-copy-markdown-link)))

(provide 'prelude-help)
