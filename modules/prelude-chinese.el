;;; -*- lexical-binding: t; -*-

;; Must-have.
(setopt word-wrap-by-category t)

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
      '("LXGW WenKai"
        "PingFang TC"))
(setq +preferred-chinese-font (car-safe +preferred-chinese-fonts))
(when (and +preferred-chinese-font
           (member +preferred-chinese-font +font-family-list))
  (set-fontset-font t 'han +preferred-chinese-font)
  (set-fontset-font t 'kana +preferred-chinese-font)
  (set-fontset-font t 'kanbun +preferred-chinese-font)
  (set-fontset-font t 'hangul +preferred-chinese-font)
  (set-fontset-font t 'cjk-misc +preferred-chinese-font)
  (set-fontset-font t 'unicode +preferred-chinese-font nil 'append))

;; font for emoji
(when k|mac
  (set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'prepend))

;; fallback font
(when (member "TH-Tshyn-P0" +font-family-list)
  (dolist (thfont '("TH-Feon" "TH-Sy-P0" "TH-Sy-P2" "TH-Sy-P16" "TH-Tshyn-P0"))
    (set-fontset-font t 'unicode thfont nil 'append)))

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
  :defer t
  :custom
  (default-input-method "rime")
  :init
  (defun rime-menu ()
    (interactive)
    (let ((last-input-event (aref (kbd "C-`") 0)))
      (rime-send-keybinding)))
  (defun rime-moran-trad ()
    (interactive)
    (rime-lib-set-option "simplification" nil))
  (defun rime-moran-simp ()
    (interactive)
    (rime-lib-set-option "simplification" t))
  (when k|mac
    (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include")))

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

(use-package emt
  :load-path "lisp/emt"
  :if k|mac
  :bind (("M-f" . emt-forward-word)
         ("M-b" . emt-backward-word))
  :config
  (setq emt-lib-path (expand-file-name "lisp/emt/module/.build/release/libEMT.dylib" user-emacs-directory))
  (emt-ensure))

(defun opencc-on-region (beg end conf)
  (let ((prefix (if (file-exists-p "/usr/local/share/opencc/s2t.json")
                    "/usr/local/share/opencc"
                  "/usr/share/opencc")))
    (shell-command-on-region beg end (format "opencc -c %s/%s.json" prefix conf))))

(defun opencc-t2s (beg end)
  "Use opencc to convert trad to simp on region."
  (interactive "r")
  (opencc-on-region beg end "t2s"))

(defun opencc-s2t (beg end)
  "Use opencc to convert simp to trad on region."
  (interactive "r")
  (opencc-on-region beg end "s2t"))

(provide 'prelude-chinese)
