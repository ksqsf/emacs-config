;; -*- lexical-binding: t; -*-

(iload org-macs org-compat org-faces org-entities org-list org-pcomplete
       org-src org-footnote org-macro ob
       org org-clock org-agenda org-capture)

(add-hook 'org-mode-hook #'visual-line-mode)

;; This config is still fledging. I don't use org-roam (for now).

;; Basic user options and special org files
(setq org-directory "~/org")
(setq org-agenda-files '("~/org"))
;; (setq org-default-notes-file "~/org/notes.org")
;; (setq +org-journal-dir (expand-file-name "journal" org-directory))
;; (setq +org-plan-file (expand-file-name "plan.org" org-directory))

;; Global keys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(defhydra hydra-org nil
  "org-mode"
  ("a" org-agenda "agenda" :exit t)
  ("c" org-capture "capture" :exit t)
  ("l" +find-ledger-file "ledger" :exit t)
  ("o" +open-org-dir "open dir" :exit t)
  ("f" +find-org "find file" :exit t)
  ("j" org-journal-new-entry "journal" :exit t))
(global-set-key (kbd "C-c o") 'hydra-org/body)
(defun +open-org-dir ()
  "Open `org-directory' in a dired buffer."
  (interactive)
  (find-file org-directory))
(defun +find-ledger-file ()
  (interactive)
  (find-file (expand-file-name "Ledger/all.dat" org-directory)))


;; Startup options
(setq org-startup-indented t)
(with-eval-after-load 'org (require 'org-tempo)) ; Legacy completion system, e.g. <s tab

;; Editing
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-return-follows-link t)
(setq org-fold-catch-invisible-edits 'smart)
(setq org-pretty-entities t)

;; Apperance
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config (setq org-hide-emphasis-markers t))

;; LaTeX preview
(setq org-latex-preview-ltxpng-directory (no-littering-expand-var-file-name "org/ltxpng"))
(setq org-preview-latex-default-process 'dvisvgm)
(when k|mac
  (with-eval-after-load 'org
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))))
(use-package org-xlatex
  :when (featurep 'xwidget-internal)
  :vc (:fetcher github :repo "ksqsf/org-xlatex")
  :after (org)
  :hook (org-mode . org-xlatex-mode)
  :hook (markdown-mode . org-xlatex-mode))

;; (org) Speed Keys
(setq org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp) (looking-back "^\\**"))))


;; Capture templates
(setq org-capture-templates
      `(("t"                                      ; keys
         "Task"                                   ; description
         entry                                    ; type
         (file+headline "plan.org" "Inbox")       ; target
         "* TODO %?
%a")
        ("j" "journal" entry (file+datetree "journal.org")
         "* %?\n%U\n%i\n%a")))


;; Babel
(defun +org-babel-execute-src-block (&optional _arg info _params)
  "Load language if needed"
  (let* ((lang (nth 0 info))
         (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
         (backup-languages org-babel-load-languages))
    ;; - (LANG . nil) 明确禁止的语言，不加载。
    ;; - (LANG . t) 已加载过的语言，不重复载。
    (unless (assoc sym backup-languages)
      (condition-case err
          (progn
            (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
            (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
        (file-missing
         (setq-default org-babel-load-languages backup-languages)
         err)))))
(advice-add 'org-babel-execute-src-block :before '+org-babel-execute-src-block)
(setq org-babel-load-languages
      '((emacs-lisp . t)
        (C . t)
        (rust . t)
        (python . t)
        (dot . t)
        (d2 . t)))
(setq org-babel-python-command "python3")
(use-package ob-d2 :defer t)
(use-package d2-mode :defer t)

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :config
  (setopt deft-recursive t
          deft-use-filter-string-for-filename t
          deft-default-extension "org"
          deft-directory org-directory
          deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
          deft-use-filename-as-title t))

(use-package org-download
  :disabled                             ; not used atm
  :after (org)
  :custom
  (org-download-image-dir "./images"))

;; Fix cdlatex
(define-advice texmathp (:before-until () org-cdlatex-fix)
  "In org-cdlatex-mode, call `org-inside-LaTeX-fragment-p'."
  (and org-cdlatex-mode (org-inside-LaTeX-fragment-p)))


;;; CTeX support
(with-eval-after-load 'org
  (require 'ox-latex)
  (setq org-latex-compiler "xelatex")
  (add-to-list 'org-latex-classes
	       '("ctexart"
		 "\\documentclass{ctexart}
[PACKAGES]
[EXTRA]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;; For logseq-open-in-emacs, see https://github.com/ksqsf/logseq-open-in-emacs
(use-package org-protocol
  :defer 10
  :ensure org
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("org-find-file" :protocol "find-file" :function org-protocol-find-file :kill-client nil))

  (defun org-protocol-find-file-fix-wsl-path (path)
    "Change Windows-style paths to WSL-style paths if inside WSL."
    (if (not (string-match-p "-[Mm]icrosoft" operating-system-release))
        path
      (save-match-data
        (if (/= 0 (string-match "^\\([a-zA-Z]\\):\\(/.*\\)" path))
            path
          (let ((volume (match-string-no-properties 1 path))
                (abspath (match-string-no-properties 2 path)))
            (format "/mnt/%s%s" (downcase volume) abspath))))))

  (defun org-protocol-find-file (fname)
    "Process org-protocol://find-file?path= style URL."
    (let* ((parsed (org-protocol-parse-parameters fname nil '(:path :anchor)))
           (f (plist-get parsed :path))
           (anchor (plist-get parsed :anchor))
           (anchor-re (and anchor (concat "\\(-\\|\\*\\) " (regexp-quote anchor)))))
      (find-file (org-protocol-find-file-fix-wsl-path f))
      (raise-frame)
      (select-frame-set-input-focus (selected-frame))
      (unhighlight-regexp t)
      (highlight-regexp anchor-re)
      (when anchor
        (or (re-search-forward anchor-re nil t 1)
            (re-search-backward anchor-re nil t 1))))))


;; Citation and Bibliography
;;
;; Usage:
;; 1. Cite with M-x org-cite-insert (C-c C-x @)
;; 2. #+PRINT_BIBLIOGRAPHY:
(use-package citeproc :after (org))
(with-eval-after-load 'org
  (require 'oc-bibtex)
  (require 'citeproc)
  (setq org-cite-global-bibliography `(,(expand-file-name "Research/all.bib" org-directory)))
  (setq org-cite-export-processors '((t csl))))

(use-package ebib
  :defer t
  :bind (("C-c e" . ebib)
         :map ebib-index-mode-map
         ("B" . ebib-biblio-import-doi))
  :custom
  (ebib-preload-bib-files `(,(expand-file-name "Research/all.bib" org-directory)))
  :config
  (require 'ebib-biblio)
  (setq ebib-reading-list-file (expand-file-name "Read.org" org-directory)))


(use-package org-preview-html
  :defer t
  :commands (org-preview-html-mode)
  :config
  (setq org-preview-html-viewer 'xwidget))


(defun +find-org ()
  "Find or create an org file."
  (interactive)
  (let ((default-directory org-directory))
    (project-find-file)))


;; I've somehow changed my mind about org-roam.  I can still keep the
;; old file-based way of working, but org-roam *adds* the ability to
;; link nodes.  So it's a win.
(use-package org-roam
  :custom
  (org-roam-directory org-directory)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))


;; (use-package org-roam
;;   :custom
;;   (org-roam-directory (file-truename "~/Documents/org"))
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ;; Dailies
;;          ("C-c n t" . org-roam-dailies-goto-today)
;;          ("C-c n j" . org-roam-dailies-capture-today))
;;   :config
;;   (require 'org-roam-protocol)
;;   (org-roam-db-autosync-mode)

;;   ;; The following are from: https://jethrokuan.github.io/org-roam-guide/
;;   ;; That workflow seems too heavyweight for me...
;;   ;; Let's see.

;;   ;; Show node types in the node list
;;   (cl-defmethod org-roam-node-type ((node org-roam-node))
;;     "Return the TYPE of NODE."
;;     (condition-case nil
;;         (file-name-nondirectory
;;          (directory-file-name
;;           (file-name-directory
;;            (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;       (error "")))
;;   (setq org-roam-node-display-template (concat "${type:15}" "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

;;   ;; Capture templates
;;   (setq org-roam-capture-templates
;;         '(("f" "fleet" plain "%?"
;;            :if-new (file+head "fleet/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;            :immediate-finish t
;;            :unnarrowed t)
;;           ("m" "main" plain "%?"
;;            :if-new (file+head "main/${slug}.org" "#+title: ${title}\n")
;;            :immediate-finish t
;;            :unnarrowed t)
;;           ("e" "essay" plain "%?"
;;            :if-new (file+head "essay/${slug}.org" "#+title: ${title}\n#+author: \n#+date: \n")
;;            :immediate-finish t
;;            :unnarrowed t)
;;           ("p" "paper" plain "%?"
;;            :if-new (file+head "paper/${slug}.org"
;;                               "#+title: ${title}\n")
;;            :immediate-finish t
;;            :unnarrowed t)
;;           ("b" "book" plain "%?"
;;            :if-new (file+head "book/${slug}.org"
;;                               "#+title: ${title}\n#+filetags: :book:\n")
;;            :immediate-finish t
;;            :unnarrowed t))))

;; (use-package org-roam-ui
;;   :after org-roam
;;   :config
;;   (defalias 'orui 'org-roam-ui-open)
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))


;; I shall discuss this with the org devs later:

;; (defvar +org-resource-dir (expand-file-name "~/MEGA"))
;; (setenv "RESOURCE" +org-resource-dir)
;; (advice-add 'org-store-link :around
;;             (lambda (oldfun &rest args)
;;               (let ((directory-abbrev-alist
;;                      (append (list (cons +org-resource-dir "$RESOURCE"))
;;                              directory-abbrev-alist)))
;;                 (apply oldfun args))))


(provide 'prelude-org)
