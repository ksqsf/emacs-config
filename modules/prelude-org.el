;; -*- lexical-binding: t; -*-
;; Recommends:
;; 1. prelude-tex: for cdlatex

(ensure-package 'org-bullets)

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
(setq org-todo-keywords '((sequence "TODO(t!)" "NEXT(n!)" "WAIT(w@/!)"
				    "OTHERS(o@/!)"
				    "|" "DONE(d!)" "CANCELLED(c!)")))

;; Agenda
(setq org-agenda-files '("~/org/todo.org"
			 "~/org/read.org"
			 "~/org/research.org"
                         "~/org/work.org"))
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-'") #'org-cycle-agenda-files)

;; Capture
(setq org-capture-templates
      '(("t" "待办" entry (file+headline "~/org/todo.org" "未分类任务")
	 "* TODO %?\n  %i\n  %a")
	("d" "今天做了什么？有什么感想？" entry (file+olp+datetree "~/org/diary.org")
	 "* %?")
	("r" "一些乱糟糟的思绪" entry (file+headline "~/org/capture.org" "随机垃圾"))
        ("n" "任何东西的记录" entry (file+olp+datetree "~/org/notes.org")
         "* %?")))
(global-set-key (kbd "C-c c") #'org-capture)

;; Enable org-cdlatex by default
(autoload 'org-cdlatex-mode "org")
(when (package-installed-p 'cdlatex)
  (add-hook 'org-mode-hook #'org-cdlatex-mode))

(with-eval-after-load 'org
  (cl-eval-when 'compile (require 'org))

  ;; Catch invisible edits!
  (setq org-catch-invisible-edits 'smart)

  ;; Better looking bullets
  (add-hook 'org-mode-hook #'org-bullets-mode)

  ;; Formulae preview
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
			       (sqlite . t)
			       (python . t))))

;; add ctex support to ox-latex
(with-eval-after-load 'ox-latex
  (cl-eval-when 'compile (require 'ox-latex))

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

(provide 'prelude-org)
