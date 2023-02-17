;; -*- lexical-binding: t; -*-
;; Recommends:
;; 1. prelude-tex: for cdlatex

(defvar k|roam-dir (expand-file-name "~/Documents/Roam"))
(defvar k|zotlib-path (expand-file-name "~/Zotero/zotlib.bib"))

(use-package org
  :defer t
  :ensure nil
  :custom
  (org-latex-preview-ltxpng-directory (no-littering-expand-var-file-name "org/ltxpng"))
  :hook (org-mode . org-cdlatex-mode)
  :hook (org-mode . visual-line-mode)
  :hook (org-mode . prelude/set-line-spacing)
  :hook (org-mode . org-indent-mode)
  :bind (("C-c a" . org-agenda)
	 ("C-'" . org-cycle-agenda-files)
	 ("C-c c" . org-capture)
         ("C-c o" . find-org-file))
  :config
  ;; (add-hook 'org-mode-hook #'valign-mode)

  ;; Maybe prettify?
  (setq org-startup-indented nil
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(300))
  (use-package org-modern
    :disabled
    :hook (org-mode . org-modern-mode))
  (defun prelude/set-line-spacing ()
    (setq line-spacing 0.2))

  ;; Auto toggle emphasis markers, like in Typora.
  (use-package org-appear
    :hook (org-mode . org-appear-mode))

  (setq org-return-follows-link t
        org-directory (expand-file-name "~/org"))
  ;; Org-directory is not really used anymore: My notes are not managed by Org-roam.

  ;; Configs of Agenda and Capture have been removed!
  ;;
  ;; GTD has moved to TickTick, and capturing of random ideas should
  ;; go into Org-roam (org-roam-dailies-capture-today) or Flomo.

  ;; Catch invisible edits!
  (setq org-catch-invisible-edits 'smart)

  ;; Formulae preview
  (setq org-preview-latex-default-process 'dvisvgm)
  (when k|mac
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75)))

  ;; Babel
  (use-package ob-rust :demand t)
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (emacs-lisp . t)
			       (sqlite . t)
			       (python . t)
                               (haskell . t)
                               (dot . t)
                               (rust . t)))

  (defun find-org-file ()
    "Find one of my org files."
    (interactive)
    (let* ((files (reverse (directory-files (expand-file-name org-directory)
                                            nil
                                            "^[^\\.].*\\.org$")))
           (file (completing-read "Org File: " files)))
      (find-file (expand-file-name file org-directory))))

  ;; Fix cdlatex
  (define-advice texmathp (:before-until () org-cdlatex-fix)
    "In org-cdlatex-mode, call `org-inside-LaTeX-fragment-p'."
    (and org-cdlatex-mode (org-inside-LaTeX-fragment-p))))

(use-package worf
  :disabled
  :hook (org-mode . worf-mode))

(use-package org-tempo
  :ensure nil
  :after (org)
  :config
  (add-to-list 'org-tempo-keywords-alist '("ra" . "ROAM_ALIAS")))

(use-package helm-bibtex
  :after (org-ref)
  :commands (helm-bibtex helm-bibtex-with-notes)
  :config
  (setq
   bibtex-completion-notes-path k|roam-dir
   bibtex-completion-bibliography k|zotlib-path
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: ${file}\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n")))

(use-package org-ref
  :commands (org-ref)
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography k|zotlib-path
   org-ref-bibliography-notes (expand-file-name "bibnotes.org" k|roam-dir)
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory k|roam-dir
   org-ref-notes-function 'orb-edit-notes))

(use-package org-download
  :after (org)
  :custom
  (org-download-image-dir "./attachments"))

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

(defvar-local k|org-last-in-latex nil)

(defun k|org-in-latex-fragment-p ()
  "Returns non-nil when the point is inside a latex fragment."
  (let* ((el (org-element-context))
         (el-type (car el)))
    (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
         (org-element-property :begin el))))

(defun k|org-latex-auto-toggle ()
  (ignore-errors
    (let ((in-latex (k|org-in-latex-fragment-p)))
      (if (and k|org-last-in-latex (not in-latex))
          (progn (org-latex-preview)
                 (setq k|org-last-in-latex nil)))

      (when-let ((ovs (overlays-at (point))))
        (when (->> ovs
                   (--map (overlay-get it 'org-overlay-type))
                   (--filter (equal it 'org-latex-overlay)))
          ;; (org-latex-preview)
          (make-thread #'(lambda () (thread-yield) (sit-for 0) (org-latex-preview)) "Async LaTeX fragment Preview")
          (setq k|org-last-in-latex nil)))

      (when in-latex
        (setq k|org-last-in-latex t)))))

(define-minor-mode org-latex-auto-toggle
  "Automatic toggle latex overlay when cursor enter/leave."
  nil nil nil
  (if org-latex-auto-toggle
      (add-hook 'post-command-hook #'k|org-latex-auto-toggle nil t)
    (remove-hook 'post-command-hook #'k|org-latex-auto-toggle t)))

(use-package org-latex-impatient
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin
        "~/node_modules/mathjax-node-cli/bin/tex2svg"))

;; As is said above, agenda is not used anymore.
;; But this code is useful as an example, so I kept it.
(use-package org-ql
  :disabled
  :after (org)
  :config
  ;; (setq org-agenda-custom-commands
  ;;       '(("#" "Stuck Projects"
  ;;          ((org-ql-block '(and (tags "project")
  ;;                               (not (done))
  ;;                               (not (descendants (todo "NEXT")))
  ;;                               (not (descendants (scheduled))))
  ;;                         ((org-ql-block-header "Stuck Projects")))))))
  )

;; org-roam
(use-package org-roam
  :commands (org-roam-node-find
             org-roam-node-random
             org-roam-dailies-goto-today
             org-roam-dailies-capture-today)
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory k|roam-dir)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n c" . org-roam-dailies-capture-today)
         :map org-mode-map
         (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n o" . k|org-get-copy-id)
          ("C-c n a t" . org-roam-tag-add)
          ("C-c n a a" . org-roam-alias-add)))
  :config
  (defun k|org-get-copy-id ()
    (interactive)
    (kill-new (org-id-get-create)))
  (org-roam-db-autosync-mode t)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new
           (file+head "${slug}.org"
                      "#+title: ${title}\n#+date: %u\n#+lastmod: \n\n")
           :immediate-finish t))
        time-stamp-start "#\\+lastmod: [\t]*")
  (add-hook 'org-mode-hook
            (lambda ()
              (setq company-backends '(company-capf company-yasnippet company-dabbrev)))))

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref))

(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; ~~The WM can handle splits~~ No it can't
   ;; org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list k|roam-dir)))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory k|roam-dir)
  (deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  (deft-use-filename-as-title t))

(use-package org-roam-ui
  :after (org-roam)
  :commands (org-roam-ui-mode org-roam-ui-open)
  :config
  ;; pull in fix from
  ;; https://github.com/org-roam/org-roam-ui/pull/214
  (defun org-roam-ui--on-msg-open-node (data)
  "Open a node when receiving DATA from the websocket."
  (let* ((id (alist-get 'id data))
          (node (org-roam-node-from-id id))
          (pos (org-roam-node-point node))
          (buf (find-file-noselect (org-roam-node-file node))))
    (run-hook-with-args 'org-roam-ui-before-open-node-functions id)
    (unless (window-live-p org-roam-ui--window)
      (if-let ((windows (window-list))
               (or-windows (seq-filter
                            (lambda (window)
                              (org-roam-buffer-p
                               (window-buffer window))) windows))
               (newest-window (car
                               (seq-sort-by
                                #'window-use-time #'> or-windows))))
          (setq org-roam-ui--window newest-window)
        (split-window-horizontally)
        (setq org-roam-ui--window (frame-selected-window))))
    (set-window-buffer org-roam-ui--window buf)
    (select-window org-roam-ui--window)
    (goto-char pos)
    (run-hook-with-args 'org-roam-ui-after-open-node-functions id))))

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
    (let ((f (plist-get (org-protocol-parse-parameters fname nil '(:path)) :path)))
      (find-file (org-protocol-find-file-fix-wsl-path f))
      (raise-frame)
      (select-frame-set-input-focus (selected-frame)))))

(provide 'prelude-org)
