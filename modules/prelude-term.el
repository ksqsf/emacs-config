;;; prelude-term.el --- In-Emacs terminal settings   -*- lexical-binding: t; -*-

;;; Code:

;; Shell and Comint
(use-package comint
  :ensure nil
  :config
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output))

;; Do not query on exit if the term process has no running child process.
(advice-add 'process-kill-buffer-query-function :before-until
            (lambda (&rest args)
              (and (derived-mode-p 'ghostel-mode 'vterm-mode 'shell-mode 'term-mode 'comint-mode 'eat-mode)
                   (and (get-buffer-process (current-buffer))
                        (not (process-running-child-p (get-buffer-process (current-buffer))))))))

;; auto exit shell-mode buffer if the shell exited
(defun shell-kill-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
    (lambda (proc event)
      (when (memq (process-status proc) '(exit signal))
        (kill-buffer (process-buffer proc))))))
(add-hook 'shell-mode-hook #'shell-kill-buffer-on-exit)

;; eshell
(use-package eshell
  :ensure nil
  :config
  ;; banner
  ;; (setq eshell-banner-message "Welcome to the Emacs shell!\n\n")

  ;; modules
  (add-to-list 'eshell-modules-list
               'eshell-xtra)

  ;; history
  (setq eshell-buffer-maximum-lines 4096
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input 'all
        eshell-destroy-buffer-when-process-dies t)

  ;; stuff
  (setq eshell-visual-commands
        '("vim" "nvim" "less" "more" "htop" "top" "man"
          "ssh" "fzf" "watch" "python" "ipython" "bash" "fish" "kak"))
  (setq eshell-visual-subcommands
        '(("git" "log" "diff" "show")
          ("ghcup" "tui")))

  ;; fish-like prompt with git branch
  ;; (defun my/eshell-git-branch ()
  ;;   (when (locate-dominating-file "." ".git")
  ;;     (let ((branch (string-trim
  ;;                    (shell-command-to-string
  ;;                     "git branch --show-current 2>/dev/null"))))
  ;;       (unless (string-empty-p branch)
  ;;         (propertize (format " (%s)" branch)
  ;;                     'face '(:foreground "#bd93f9"))))))
  ;; (defun my/eshell-prompt ()
  ;;   (let* ((dir   (abbreviate-file-name (eshell/pwd)))
  ;;          (parts (split-string dir "/"))
  ;;          (short (if (> (length parts) 3)
  ;;                     (concat "…/" (string-join (last parts 2) "/"))
  ;;                   dir))
  ;;          (root? (= (user-uid) 0)))
  ;;     (concat "\n"
  ;;             (propertize short 'face '(:foreground "#8be9fd" :weight bold))
  ;;             (or (my/eshell-git-branch) "")
  ;;             "\n"
  ;;             (propertize (if root? "# " "❯ ")
  ;;                         'face `(:foreground ,(if root? "#ff5555" "#50fa7b")
  ;;                                             :weight bold)))))

  ;; (setq eshell-prompt-function #'my/eshell-prompt
  ;;       eshell-prompt-regexp    "^[❯#] ")

  ;; fish-like `up' to go up N directories
  (defun eshell/up (&optional n)
    (eshell/cd (string-join (make-list (or n 1) "..") "/")))

  ;; fish-like abbr
  (setq eshell-aliases-file (expand-file-name "~/.emacs.d/etc/eshell/aliases")))

(use-package eshell-z
  :after eshell
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

;; Coterm
(use-package coterm
  :hook (after-init . coterm-mode))

;; Ghostel
(use-package ghostel
  :bind
  ( :map ghostel-semi-char-mode-map
    ("C-q" . ghostel-send-next-key)
    ("M-o" . nil)
    ("M-s" . nil)))

;; Dropdown terminal
(defun drop-down-term ()
  "Open a drop-down terminal in the same directory as the current file."
  (interactive)
  (let ((ghostel-buffer-name "*dd-term*")
        buffer win)
    (ghostel)
    (previous-buffer)
    (setq buffer (get-buffer "*dd-term*"))
    (setq win
          (display-buffer-in-side-window
           buffer
           '((side . top)
             (dedicated . t))))
    (select-window win)))

(defalias 'dd-term 'drop-down-term)
(defalias 'ddt 'drop-down-term)

(use-package kitty-graphics
  :if (not (display-graphic-p))
  :vc (:fetcher github :repo "cashmeredev/kitty-graphics.el"))

(provide 'prelude-term)
;;; prelude-term.el ends here
