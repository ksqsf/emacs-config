;;;
;;; Early Appearance (to avoid flickers)
;;;
(scroll-bar-mode -1)
(column-number-mode 1)
;; (menu-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(when (daemonp)
  (menu-bar-mode -1)
  (tool-bar-mode -1))


;;;
;;; Emacs itself
;;;
(setq gc-cons-threshold 100000000)
(global-unset-key (kbd "C-z"))
(add-to-list 'load-path (expand-file-name "contrib" user-emacs-directory))
(setq-default bidi-display-reordering nil)


;;;
;;; WC ring
;;;
(defvar wc-ring nil)
(defvar wc-ring-max 10)

(defun push-wc ()
  (if (not (null (get-register ?9)))
      (set-register ?0 (get-register ?9)))
  (dolist (i '(?9 ?8 ?7 ?6 ?5 ?4 ?3 ?2))
    (if (not (null (get-register (1- i))))
	(set-register i (get-register (1- i)))))
  (window-configuration-to-register ?1))

(defun go-to-home ()
  "Quickly go back to scratch.  Previous window configuration is
saved in register 1."
  (interactive)
  (if (not (equal (current-window-configuration) (get-register ?1)))
      (push-wc))
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (erase-buffer)
  (insert (scratch-message))
  (set-buffer-modified-p nil))

(global-set-key (kbd "<home>") #'go-to-home)


;;;
;;; Package
;;;
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org" . "http://elpa.emacs-china.org/org/")))
(package-initialize)
(eval-when-compile
  (require 'use-package))



;;;
;;; Appearance
;;;
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))

;; Do my own modeline...


(require 'ansi-color)
(defalias 'list-buffers 'ibuffer)

;; (desktop-save-mode t)

(defun renew-scratch-message ()
  (interactive)
  (save-excursion
    (get-buffer-create "*scratch*")
    (with-current-buffer "*scratch*"
      (lisp-interaction-mode)
      (erase-buffer)
      (insert (scratch-message))
      (goto-char (point-max)))))
(run-with-idle-timer 600 t #'renew-scratch-message)

(defun scratch-message ()
  (format "%s\n;; Happy Hacking!
;; Don't forget to check your calendar! (C-c a)\n\n"
	  (with-temp-buffer
	    (lisp-interaction-mode)
	    (insert (shell-command-to-string "fortune"))
	    (comment-region (point-min) (point-max))
	    (ansi-color-apply-on-region (point-min) (point-max))
	    (buffer-string))))

(setq inhibit-startup-screen t)
(setq initial-scratch-message (scratch-message))

(defun my-toggle-fullscreen ()
  (interactive)
  (toggle-frame-fullscreen))

(global-set-key (kbd "<f11>") #'my-toggle-fullscreen)

(defun set-selected-frame-dark ()
  (interactive)
  (let ((frame-name (cdr (assoc 'name (frame-parameters)))))
    (call-process-shell-command (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -name \""
                                        frame-name
                                        "\""))))

(add-hook 'after-init-hook #'set-selected-frame-dark)

(use-package all-the-icons
  :ensure t
  :defer 2)

;(set-frame-parameter nil 'alpha 0.95)


;;;
;;; Region
;;;
(defvar region-ring nil
  "The global region ring.  Each element of it is like 
(buffer mark point).")
(defvar region-ring-max 60
  "The maximum size of the global region ring.")

(defun push-region ()
  "Push the current region into the global region ring.  The
current mark will be popped off the mark ring."
  (interactive)
  (let ((buffer (current-buffer))
	(mark (copy-marker (mark-marker)))
	(point (point-marker)))
    (if (or (null mark)
	    (null point))
	(error "Mark not set; no region available")
      (save-excursion
	(pop-mark)
	(let ((tail (if (= (length region-ring) region-ring-max)
			(cdr region-ring)
		      region-ring)))
	  (setq region-ring (cons (list buffer mark point) tail)))))))

(defun pop-region ()
  "Pop off region ring and go to the actual position."
  (interactive)
  (if (null region-ring)
      (error "Region ring is empty!")
    (let* ((top (pop region-ring))
	   (buffer (car top))
	   (mark (cadr top))
	   (point (caddr top)))
      (switch-to-buffer buffer)
      (push-mark)
      (set-mark mark)
      (goto-char point))))

(global-set-key (kbd "<f5>") #'push-region)
(global-set-key (kbd "<f6>") #'pop-region)


;;;
;;; Input methods
;;;
(use-package chinese-wbim
  :defer 2
  :config
  (register-input-method
   "chinese-wbim" "euc-cn" 'chinese-wbim-use-package
   "五笔" "汉字五笔输入法" "wb.txt"))


;;;
;;; Neotree
;;;
(use-package neotree
  :defer t
  :commands (neotree-toggle)
  :bind (("<f9>" . neotree-toggle))
  :after all-the-icons
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t))


;;; 
;;; Org mode
;;; 
(use-package org
  :ensure t
  :defer t
  :mode (("\\.org\\'" . org-mode)
	 ("\\.org_archive\\'" . org-mode))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb)
	 ("C-c c" . org-capture)
	 ("C-'" . org-cycle-agenda-files))
  :init
  (require 'org-mouse)
  :config
  (setq org-ellipsis "↩")
  (setq org-capture-templates
	'(("t" "待办" entry (file+headline "~/org/todo.org" "未分类任务")
	   "* TODO %?\n  %i\n  %a")
	  ("d" "今天做了什么？有什么感想？" entry (file+olp+datetree "~/org/diary.org")
	   "* %?")
	  ("r" "一些乱糟糟的思绪" entry (file+headline "~/org/capture.org" "随机垃圾"))))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (set-face-attribute 'org-table nil :family "Sarasa Term TC"))

(use-package ox-twbs
  :ensure t
  :after org)

(use-package ox-reveal
  :ensure t
  :after org)

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))


;;;
;;; Backups
;;;
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))


;;;
;;; Efficiency for Window Manipulation
;;;
(use-package windmove
  :defer 1
  :config
  (windmove-default-keybindings))

(use-package winum
  :ensure t
  :config
  (global-set-key (kbd "s-0") #'winum-select-window-0)
  (global-set-key (kbd "s-1") #'winum-select-window-1)
  (global-set-key (kbd "s-2") #'winum-select-window-2)
  (global-set-key (kbd "s-3") #'winum-select-window-3)
  (global-set-key (kbd "s-4") #'winum-select-window-4)
  (global-set-key (kbd "s-5") #'winum-select-window-5)
  (global-set-key (kbd "s-6") #'winum-select-window-6)
  (global-set-key (kbd "s-7") #'winum-select-window-7)
  (global-set-key (kbd "s-8") #'winum-select-window-8)
  (global-set-key (kbd "s-9") #'winum-select-window-9)
  (winum-mode 1))


;;;
;;; Efficiency for Command Invocation
;;;
(icomplete-mode 1)
(ido-mode 1)
(ido-everywhere 1)

(use-package smex
  :ensure t
  :defer t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

;; (use-package ivy :ensure t
;;   :diminish (ivy-mode . "")
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t))

;; (ido-mode 1)
;; (setq ido-use-filename-at-point 'guess)

;; (use-package ido-vertical-mode
;;   :ensure t
;;   :config
;;   ;; (ido-vertical-mode 1)
;;   ;; (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
;;   )

;; (use-package flx-ido
;;   :ensure t
;;   :config
;;   (flx-ido-mode 1)
;;   (setq ido-enable-flex-matching nil)
;;   (setq flx-ido-use-faces nil))



;;;
;;; Efficiency for Find Files
;;;
(global-set-key (kbd "C-x C-M-f") 'find-file-at-point)


;;;
;;; Recent Files
;;;
(use-package recentf
  :bind (("<f7>" . recentf-open-files))
  :config
  (recentf-mode 1))


;;;
;;; Editing Facilities
;;;
(use-package multiple-cursors
  :ensure t
  :defer 2
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))


;;;
;;; Search
;;;
(use-package ripgrep
  :ensure t
  :defer 2
  :commands rg ripgrep-regexp
  :config (defalias 'rg 'ripgrep-regexp))


;;;
;;; Discoverability
;;;
(use-package which-key
  :ensure t
  :defer 1
  :diminish ""
  :config
  (which-key-mode))

(use-package helpful
  :defer 1
  :commands (helpful-function helpful-command helpful-key helpful-macro helpful-callable helpful-variable helpful-at-point)
  :ensure t
  :bind (("C-h ." . helpful-at-point)
	 ("C-h f" . helpful-function)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)))


;;;
;;; Project Management
;;;
(use-package projectile-ripgrep
  :ensure t
  :commands projectile-ripgrep)

(use-package projectile
  :ensure t
  :diminish ""
  :after hydra
  :bind (("C-c p p" . projectile-switch-project)
	 ("C-c p f" . projectile-find-file)
	 ("C-c p g" . projectile-ripgrep)
	 ("C-c p k" . projectile-kill-buffers)
	 ("C-c p r" . projectile-replace)
	 ("C-c p v" . projectile-vc)
	 ;; ("C-c p b" . projectile-compile-project)
	 ("C-c p t" . projectile-test-project)
	 ("C-c p RET" . projectile-run-shell)
	 ("C-c p o" . projectile-find-other-file)
	 ("<menu>" . projectile-commander))
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'hybrid)
  ;; (setq projectile-enable-caching t)
  (setq projectile-switch-project-action #'projectile-find-dir)
  (setq projectile-find-dir-includes-top-level t))

(when (fboundp 'winner-mode)
  (winner-mode 1))

(defun google (keyword)
  (interactive "sKeyword: ")
  (browse-url (format "https://google.com/search?q=%s" keyword)))


;;;
;;; Additional Editing Commands
;;;
(defun mark-line ()
  "Mark the current line."
  (interactive)
  (end-of-line)
  (push-mark (line-beginning-position))
  (exchange-point-and-mark))

(defun mark-contiguous-heading (heading)
  "Mark a series of contiguous lines whose first column are the same."
  (interactive "sMark lines starting with: ")
  (push-mark (line-beginning-position))
  (while (string-prefix-p heading (thing-at-point 'line t))
    (forward-line)
    (beginning-of-line))
  (exchange-point-and-mark))

(global-set-key
 (kbd "C-x m")
 (defhydra hydra-mark (global-map "C-x m")
   "mark"
   ("l" mark-line "line")
   ("h" mark-contiguous-heading "head")))


;;;
;;; Auto Completion
;;;
(use-package company
  :ensure t
  :diminish company-mode
  ; :functions company-mode
  :config
  ;(global-company-mode)
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t))


;;;
;;; Syntax checking and spelling checking
;;;
(use-package flycheck
  :ensure t
  :defer t
  :diminish ""
  :commands flycheck-mode)

(add-hook 'text-mode-hook #'flyspell-mode)


;;;
;;; Snippets
;;;
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode))



;;;
;;; Git
;;;
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status)
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)))


;;;
;;; Document Viewing and Editing
;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package pdf-tools
  :ensure t
  :commands pdf-tools-install
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-tools-install))


;;;
;;; Lisp
;;;
(use-package paredit
  :ensure t
  :diminish ""
  :commands enable-paredit-mode)

(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)


;;;
;;; C/C++
;;;
(push '("\\.h\\'" . c++-mode) auto-mode-alist)

(defun my-cc-common ()
  "Initialize my CC mode working environment."
  (electric-pair-mode)
  (setq c-default-style "linux"
  	c-basic-offset 4
  	tab-width 8
  	indent-tabs-mode nil)
  (local-set-key (kbd "C-c m") 'imenu))

(add-hook 'c-mode-common-hook 'my-cc-common)

(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package disaster
  :ensure t
  :after cc-mode
  :config
  (define-key c-mode-base-map (kbd "C-c d") 'disaster))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
						  irony-cdb-clang-complete))

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package company-irony
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package irony-eldoc
  :ensure t
  :after irony
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package company-irony-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(autoload 'acm-mode "~/.emacs.d/acm/acm.el")


;;;
;;; Rust
;;;
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (defun my-init-rust-mode ()
    (company-mode)
    (yas-minor-mode)
    (cargo-minor-mode)
    (racer-mode)
    ;; (lsp-rust-enable)
    (flycheck-mode))
  (add-hook 'rust-mode-hook 'my-init-rust-mode))

(use-package cargo
  :ensure t
  :diminish cargo-minor-mode
  :after rust-mode)

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package lsp-rust
;;   :ensure t
;;   :after rust-mode)

(use-package racer
  :ensure t
  :after rust-mode
  :diminish racer-mode
  :config
  (define-key rust-mode-map [C-mouse-1] 'racer-find-definition)
  (define-key rust-mode-map [f1] 'racer-describe)
  (add-hook 'racer-mode-hook #'eldoc-mode))


;;;
;;; Python
;;;
(use-package python
  :ensure t
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :config
  (defun my-init-python ()
    ;; (elpy-enable)
    (flycheck-mode))
  (add-hook 'python-mode-hook #'my-init-python))

(use-package elpy
  :ensure t
  :after python
  :config
  (elpy-enable))


;;;
;;; TeX and LaTeX
;;;
(use-package auctex
  :ensure auctex
  :defer t
  :mode (("\\.tex$" . latex-mode))
  :config
  (setq-default TeX-engine 'xetex))


;;;;
;;;; Coq
;;;;
(setq-default proof-splash-enable nil)
;; (add-hook 'coq-mode-hook 'tool-bar-mode)
(add-hook 'coq-mode-hook
	  (lambda ()
	    (local-set-key [?\C-z] #'proof-assert-next-command-interactive)
	    (local-set-key (kbd "C-Z") #'proof-undo-last-successful-command)))


;;;
;;; Haskell
;;;
;; (use-package intero
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode))


;;;
;;; Prolog
;;;
(push '("\\.pl$" . prolog-mode) auto-mode-alist)


;;;
;;; ANTLR
;;;
(autoload 'antlr-v4-mode "antlr-mode" nil t)
(push '("\\.g4\\'" . antlr-v4-mode) auto-mode-alist)


;;;
;;; Bison Flex
;;;
(require 'bison)
(require 'flex)
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
(autoload 'bison-mode "bison")
(autoload 'flex-mode "flex")


;;;
;;; X86 ASM
;;;
(add-to-list 'auto-mode-alist '("\\.asm$" . asm86-mode))
(autoload 'asm86-mode "asm86-mode.elc")


;;;
;;; HTML or XML
;;;
(defun insert-tag (tag)
  "Insert an opening tag."
  (interactive "sTag: ")
  (let ((open-tag (format "<%s>" tag))
	(close-tag (format "</%s>" tag)))
    (insert open-tag)
    (insert close-tag)
    (backward-char (length close-tag))))

(defun insert-self-closing-tag (tag)
  "Insert a self-closing tag."
  (interactive "sTag: ")
  (insert (format "<%s />" tag)))
(global-set-key (kbd "C-c t") #'insert-tag)
(global-set-key (kbd "C-c T") #'insert-self-closing-tag)


;;;
;;; Read RFCs
;;;
(defun rfc (number)
  "Read RFC NUMBER."
  (interactive "nRFC number: ")
  (find-file (format "/usr/share/doc/RFC/links/rfc%d.txt.gz" number)))


;;;
;;; StarDict
;;;
(use-package chinese-yasdcv
  :ensure t
  :commands (yasdcv-translate-input yasdcv-translate-at-point))


;;;
;;; Additional major modes
;;;
(use-package mips-mode
  :ensure t
  :mode "\\.mips$")

(use-package toml-mode
  :ensure t
  :mode "\\.toml$")

(use-package restclient
  :ensure t
  :commands restclient-mode)

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot$")


;;;
;;; Fun
;;;
;; (use-package nyan-mode
;;   :config
;;   (nyan-mode))


;;;
;;; Blog management
;;;
(defvar my-blog-posts-dir (expand-file-name "~/Site/jekyll/_posts/"))

(defun blog--header (layout title time)
  (let ((datetime (format-time-string "%Y-%m-%d %H:%M:%S" time)))
    (format "---
layout: %s
title: \"%s\"
date: %s
---
" layout (replace-regexp-in-string "\"" "\\\\\"" title) datetime)))

(defun blog-new-post (title permalink)
  (interactive "sTitle: \nsPermalink for post '%s': \n")
  (let* ((time (current-time))
	 (date (format-time-string "%Y-%m-%d" time))
	 (filename (format "%s-%s.md" date permalink))
	 (header (blog--header "layout" title time)))
    (find-file (expand-file-name filename my-blog-posts-dir))
    (insert header)
    (newline)))



;;;
;;; Fancy Box Comment
;;;
(defun box-comment (start end)
  (interactive "r")
  (let* ((content (buffer-substring start end))
	 (mode major-mode)
	 (stripe (make-string fill-column ?*))
	 (comment (with-temp-buffer
		    (funcall mode)
		    (insert stripe "\n")
		    (insert content)
		    (goto-char (point-max))
		    (insert stripe)
		    (center-region (point-min) (point-max))
		    (comment-region (point-min) (point-max))
		    (buffer-substring (point-min) (point-max)))))
    (delete-region start end)
    (insert comment)))



;;;
;;; "Easy" Customization
;;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)



;;;
;;; Permanently enable disabled commands (because I use them)
;;;
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
