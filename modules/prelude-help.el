;; -*- lexical-binding: t; -*-

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h x" . helpful-command))
  :config
  (defun advice-helpful--navigate (button)
    "Do not use `find-file' to open a buffer."
    (pop-to-buffer (find-file-noselect (substring-no-properties (button-get button 'path))))
    (-when-let (pos (get-text-property button 'position
                                       (marker-buffer button)))
      (helpful--goto-char-widen pos)))
  (advice-add 'helpful--navigate :override 'advice-helpful--navigate))

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

(defvar Info-special-url-format
  '(("hyperbole" . "https://www.gnu.org/software/hyperbole/man/hyperbole.html#%s")
    ("magit" . "https://magit.vc/manual/magit.html#%s")
    ("forge" . "https://magit.vc/manual/forge.html#%s")
    ("transient" . "https://magit.vc/manual/transient.html#%s")
    ("org" . "https://orgmode.org/manual/%s.html")
    ("org-roam" . "https://www.orgroam.com/manual.html#%s")
    ("orderless" . "https://elpa.gnu.org/packages/doc/orderless.html#%s"))
  "`Info-get-online-url' defaults to GNU's official manual URL base.
%s will be replaced by the node name.")

(defun Info-get-online-url ()
  "Get the online URL of this manual node.

If a region is active, it is considered text anchor.  This
feature is incomplete."
  (let* ((file (file-name-sans-extension
	        (file-name-nondirectory Info-current-file)))
         (node (mapconcat (lambda (ch)
                            (if (or (< ch 32)        ; ^@^A-^Z^[^\^]^^^-
                                    (<= 33 ch 47)    ; !"#$%&'()*+,-./
                                    (<= 58 ch 64)    ; :;<=>?@
                                    (<= 91 ch 96)    ; [\]_`
                                    (<= 123 ch 127)) ; {|}~ DEL
                                (format "_00%x" ch)
                              (char-to-string ch)))
                          Info-current-node ""))
         (node (string-replace " " "-" node))
         (node (if (string= node "Top") "index"
                 node))
         (url-base (or (cdr (assoc file Info-special-url-format #'equal))
                       (format "https://www.gnu.org/software/emacs/manual/html_node/%s/%%s.html" file)))
         (url (format url-base node)))
    (if (not (use-region-p))
        url
      (let* ((selected (buffer-substring-no-properties (region-beginning) (region-end)))
             (text-anchor (mapconcat (lambda (ch)
                                       (if (or (= ch 10)
                                               (= ch 8220) (= ch 8221))
                                           ""
                                         (if (= ch ?-)
                                             "%2D"
                                           (char-to-string ch))))
                                     selected)))
        (format "%s#:~:text=%s" url (url-encode-url text-anchor))))))

(defun Info-copy-online-url ()
  "Put the online url of the current Info node into the kill ring.

If a region is active, it is considered text anchor.

This command is only meaningful for the official manuals, and it
does not work in TOC nodes."
  (interactive)
  (let ((url (Info-get-online-url)))
    (kill-new url)
    (message "%s" url)))

(defun Info-copy-markdown-link ()
  "Put a markdown link to the online manual into the kill ring.

The link is obtained as if it was returned from
`Info-copy-online-url', and the label
`Info-copy-current-node-name'."
  (interactive)
  (let* ((file (file-name-sans-extension
	        (file-name-nondirectory Info-current-file)))
         (node Info-current-node)
         (url (Info-get-online-url))
         (md-link (format "[(info \"(%s) %s\")](%s)" file node url)))
    (kill-new md-link)
    (message "%s" md-link)))

(use-package info
  :ensure nil
  :bind (:map
         Info-mode-map
         ("C" . Info-copy-online-url)
         ("C-c C-c" . Info-copy-markdown-link)))

(use-package info-colors
  :after (info)
  :defer t
  :hook (Info-selection . info-colors-fontify-node))

(provide 'prelude-help)
