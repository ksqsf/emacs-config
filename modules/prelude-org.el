;; -*- lexical-binding: t; -*-
;; Recommends:
;; 1. prelude-tex: for cdlatex

(use-package org-bullets
  :disabled
  :hook (org-mode . org-bullets-mode))

;; Productivity Problems:
;; 1. Trackable activities: I want to record my life.
;; 2. Avoid procrastination.
;; 3. Make better use of time.

;; My workflow explained.
;;
;; TODO: Something to be done
;; NEXT: I've begun working on it
;; OTHERS: others are doing it but I'm keeping an eye
;; WAIT: wait for preconditions to proceed

;; Todo tracking
;; See https://changelog.complete.org/archives/9877-emacs-3-more-on-org-mode
;; ! -> timestamps
;; @ -> additional notes
(use-package org
  :defer 1
  ; :hook (org-mode . org-cdlatex-mode)
  :hook (org-mode . visual-line-mode)
  ; :hook (org-mode . org-variable-pitch-minor-mode)
                                        ; :hook (org-mode . org-starless-mode)
                                        ; :hook (org-mode . org-padding-mode)
  :bind (("C-c a" . org-agenda)
	 ("C-'" . org-cycle-agenda-files)
	 ("C-c c" . org-capture)
         ("C-c o" . find-org-file))
  :config
  ;; (add-hook 'org-mode-hook #'valign-mode)
  ; (add-hook 'org-mode-hook #'org-indent-mode)

  ;; Maybe prettify?
  (setq org-hide-emphasis-markers t)

  (setq org-return-follows-link t
        org-directory (expand-file-name "~/org"))

  ;; Agenda
  (setq org-log-done 'time)
  (setq org-todo-keywords '((sequence "TODO(t!)" "GOAL(g!)"
				      "|" "DONE(d!)" "CANCELLED(c!)")))

  ;; Capture
  (setq org-capture-templates
	'(("T" "未分类待办" entry (file+headline "~/org/todo.org" "未分类任务")
           "* TODO %?\n  %i\n  %a")
          ("t" "待办" entry (file+olp+datetree "~/org/todo.org" "今天")
           "* TODO %?\n" :tree-type 'week)
          ("n" "记笔记" plain (clock))
          ("d" "今天做了什么？有什么感想？" entry (file+olp+datetree "~/org/diary.org")
           "* %?")
          ("r" "一些乱糟糟的思绪" entry (file+headline "~/org/capture.org" "随机垃圾"))
          ("n" "任何东西的记录" entry (file+olp+datetree "~/org/notes.org")
           "* %?")))

  ;; Catch invisible edits!
  (setq org-catch-invisible-edits 'smart)

  ;; Formulae preview
  (setq org-preview-latex-default-process 'dvisvgm)
  (when (not *is-a-mac*)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

  ;; Babel
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

(use-package org-tempo
  :ensure nil
  :after (org)
  :config
  (add-to-list 'org-tempo-keywords-alist '("ra" . "ROAM_ALIAS")))

(use-package org-ref
  :commands (org-ref))

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

(defvar-local k/org-last-in-latex nil)

(defun k/org-in-latex-fragment-p ()
  "Returns non-nil when the point is inside a latex fragment."
  (let* ((el (org-element-context))
         (el-type (car el)))
    (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
         (org-element-property :begin el))))

(defun k/org-latex-auto-toggle ()
  (ignore-errors
    (let ((in-latex (k/org-in-latex-fragment-p)))
      (if (and k/org-last-in-latex (not in-latex))
          (progn (org-latex-preview)
                 (setq k/org-last-in-latex nil)))

      (when-let ((ovs (overlays-at (point))))
        (when (->> ovs
                   (--map (overlay-get it 'org-overlay-type))
                   (--filter (equal it 'org-latex-overlay)))
          ;; (org-latex-preview)
          (make-thread #'(lambda () (thread-yield) (sit-for 0) (org-latex-preview)) "Async LaTeX fragment Preview")
          (setq k/org-last-in-latex nil)))

      (when in-latex
        (setq k/org-last-in-latex t)))))

(define-minor-mode org-latex-auto-toggle
  "Automatic toggle latex overlay when cursor enter/leave."
  nil
  nil
  nil
  (if org-latex-auto-toggle
      (add-hook 'post-command-hook #'k/org-latex-auto-toggle nil t)
    (remove-hook 'post-command-hook #'k/org-latex-auto-toggle t)))

(use-package org-latex-impatient
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin
        "~/node_modules/mathjax-node-cli/bin/tex2svg"))

(use-package org-ql
  :after (org)
  :config
  (setq org-agenda-custom-commands
        '(("#" "Stuck Projects"
           ((org-ql-block '(and (tags "project")
                                (not (done))
                                (not (descendants (todo "NEXT")))
                                (not (descendants (scheduled))))
                          ((org-ql-block-header "Stuck Projects"))))))))

(use-package org-padding
  :disabled
  :quelpa (org-padding :repo "TonCherAmi/org-padding" :fetcher github)
  :config
  (setq org-padding-block-begin-line-padding '(2.0 . nil))
  (setq org-padding-block-end-line-padding '(nil . 1.0))
  (setq org-padding-heading-padding-alist
        '((4.0 . 1.5) (3.0 . 0.5) (3.0 . 0.5) (3.0 . 0.5) (2.5 . 0.5) (2.0 . 0.5) (1.5 . 0.5) (0.5 . 0.5))))

(use-package org-starless
  :disabled
  :quelpa (org-starless :repo "TonCherAmi/org-starless" :fetcher github))

(provide 'prelude-org)
