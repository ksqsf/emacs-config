;; Recommends:
;; 1. prelude-tex: for cdlatex

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
			 "~/org/research.org"))
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-'") #'org-cycle-agenda-files)

;; Capture
(setq org-capture-templates
      '(("t" "待办" entry (file+headline "~/org/todo.org" "未分类任务")
	 "* TODO %?\n  %i\n  %a")
	("d" "今天做了什么？有什么感想？" entry (file+olp+datetree "~/org/diary.org")
	 "* %?")
	("r" "一些乱糟糟的思绪" entry (file+headline "~/org/capture.org" "随机垃圾"))))
(global-set-key (kbd "C-c c") #'org-capture)

;; Enable org-cdlatex by default
(autoload 'org-cdlatex-mode "org")
(when (package-installed-p 'cdlatex)
  (add-hook 'org-mode-hook #'org-cdlatex-mode))

;; Better looking bullets
(with-eval-after-load "org"
  (org-bullets-mode))

;; LaTeX preview fragments should be larger and clearer
(with-eval-after-load "org"
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

(provide 'prelude-org)
