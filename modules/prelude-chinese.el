;;; -*- lexical-binding: t; -*-

;; Must-have.
(setq word-wrap-by-category t)

;; Table font for perfect alignment.
;; Warning: this may not work on different font settings.
(with-eval-after-load 'org
  (set-face-font 'org-table "Iosevka Term-14")
  (set-face-font 'org-todo "Iosevka Term-14")
  (set-face-font 'org-done "Iosevka Term-14"))

(with-eval-after-load 'markdown
  (set-face-font 'markdown-table-face "Iosevka Term-14"))

(defconst +font-family-list (font-family-list)
  "A cache of the list of known font families on startup.")

;; font for Chinese texts
(setq +preferred-chinese-fonts
      '("PingFang TC"
        "LXGW WenKai"))
(setq +preferred-chinese-font (car-safe +preferred-chinese-fonts))
(when (and +preferred-chinese-font
           (member +preferred-chinese-font +font-family-list))
  (set-fontset-font t 'chinese-gbk +preferred-chinese-font))

;; Hey, Org mode?
(with-eval-after-load 'org
  (setq org-emphasis-regexp-components 
        (list (concat " \t('\"{"            "[:alpha:]") 
              (concat "- \t.,:!?;'\")}\\["  "[:alpha:]") 
              " \t\r\n,\"'" 
              "." 
              1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (setcar (nthcdr 1 org-emphasis-regexp-components) "-[:space:].。,，:；!！?？;；'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

;; Even though I speak Chinese, please use English dictionary for
;; spell checking.
(setq ispell-dictionary "en_US")

;; Overwrite the builtin version, to stop `join-line' (alias of
;; `delete-indentation') insert whitespace between CJK characters.
(define-advice k|fixup-whitespace (:override ())
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|$\\|\\s)\\|\\cc\\|\\cj\\|\\ch")
	    (save-excursion (forward-char -1)
			    (looking-at "$\\|\\s(\\|\\s'\\|\\cc\\|\\cj\\|\\ch")))
	nil
      (insert ?\s))))

;; Pinyin support for Ivy
;; Shamelessly stolen from Centaur Emacs
(use-package pinyinlib
  :commands pinyinlib-build-regexp-string
  :after ivy
  :init
  (with-no-warnings
    (defun ivy--regex-pinyin (str)
      "The regex builder wrapper to support pinyin."
      (or (pinyin-to-utf8 str)
          (and (fboundp 'ivy-prescient-non-fuzzy)
               (ivy-prescient-non-fuzzy str))
          (ivy--regex-plus str)))

    (defun my-pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*") ".*")
            (t (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
      (cond ((equal str " ") ".*")
            ((equal str "") nil)
            (t str)))

    (defun pinyin-to-utf8 (str)
      "Convert STR to UTF-8."
      (cond ((equal 0 (length str)) nil)
            ((equal (substring str 0 1) "!")
             (mapconcat
              #'my-pinyinlib-build-regexp-string
              (remove nil (mapcar
                           #'my-pinyin-regexp-helper
                           (split-string
                            (replace-regexp-in-string "!" "" str )
                            "")))
              ""))
            (t nil)))

    (mapcar
     (lambda (item)
       (let ((key (car item))
             (value (cdr item)))
         (when (member value '(ivy-prescient-non-fuzzy
                               ivy--regex-plus))
           (setf (alist-get key ivy-re-builders-alist)
                 #'ivy--regex-pinyin))))
     ivy-re-builders-alist)))

;; rime
(use-package rime
  :if (file-exists-p "~/.emacs.d/librime/dist")
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist"))

;; use rime for search
(use-package rime-regexp
  :load-path "lisp/rime-regexp"
  :after (rime)
  :commands (rime-regexp-mode)
  :config
  (rime-regexp-mode 1))

;; Define category O for Unicode's private use areas
(defvar unicode-pua-ranges
  '((#xE000 . #xF8FF)
    (#xF0000 . #xFFFFD)
    (#x100000 . #x10FFFD))
  "List of ranges of Unicode's private use areas.")
(define-category ?O "Other (PUA)")
(mapc (lambda (range)
        (modify-category-entry range ?O))
      unicode-pua-ranges)

(provide 'prelude-chinese)
