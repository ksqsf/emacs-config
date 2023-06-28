;; -*- lexical-binding: t; -*-

;; This config is still fledging. I don't use org-roam (for now).

;; Basic user options and special org files
(setq org-directory "~/Documents/org")
(setq org-agenda-files '("~/Documents/org"))
(setq org-default-notes-file "~/Documents/org/notes.org")
(setq +org-journal-dir (expand-file-name "journal" org-directory))
(setq +org-plan-file (expand-file-name "plan.org" org-directory))

;; Global keys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(defhydra hydra-org nil
  "org-mode"
  ("a" org-agenda "agenda" :exit t)
  ("c" org-capture "capture" :exit t)
  ("l" org-store-link "store link" :exit t)
  ("o" +open-org-dir "open dir" :exit t)
  ("j" org-journal-new-entry "journal" :exit t))
(global-set-key (kbd "C-c o") 'hydra-org/body)
(defun +open-org-dir ()
  "Open `org-directory' in a dired buffer."
  (interactive)
  (find-file org-directory))


;; Editing
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-return-follows-link t)
(setq org-fold-catch-invisible-edits 'smart)
(setq org-pretty-entities t)
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-appear                 ; Typora-style editing
  :defer t
  :hook (org-mode . org-appear-mode))

;; LaTeX preview
(setq org-latex-preview-ltxpng-directory (no-littering-expand-var-file-name "org/ltxpng"))
(setq org-preview-latex-default-process 'dvisvgm)
(when k|mac
  (with-eval-after-load 'org
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))))

;; Restore <s tab
(with-eval-after-load 'org
  (require 'org-tempo))


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
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-directory)
  (deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  (deft-use-filename-as-title t))

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
    (let ((f (plist-get (org-protocol-parse-parameters fname nil '(:path)) :path)))
      (find-file (org-protocol-find-file-fix-wsl-path f))
      (raise-frame)
      (select-frame-set-input-focus (selected-frame)))))


;; Citation and Bibliography
;;
;; Usage:
;; 1. Cite with M-x org-cite-insert (C-c C-x @)
;; 2. #+PRINT_BIBLIOGRAPHY:
(with-eval-after-load 'org
  (require 'oc-bibtex)
  (require 'citeproc)
  (setq org-cite-global-bibliography `(,(expand-file-name "~/Zotero/library.bib")))
  (setq org-cite-export-processors '((t csl))))

(use-package ebib
  :defer t
  :bind (("C-c e" . ebib)
         :map ebib-index-mode-map
         ("B" . ebib-biblio-import-doi)
         :map biblio-selection-mode-map
         ("e" . ebib-biblio-selection-import))
  :custom
  (ebib-preload-bib-files '("~/Zotero/library.bib"))
  :config
  (require 'ebib-biblio))

(provide 'prelude-org)
