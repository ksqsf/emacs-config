;;; prelude-term.el --- In-Emacs terminal settings   -*- lexical-binding: t; -*-

;;; Code:

;; Do not query on exit if the term process has no running child process.
(advice-add 'process-kill-buffer-query-function :before-until
            (lambda (&rest args)
              (and (derived-mode-p 'vterm-mode 'shell-mode 'term-mode 'comint-mode 'eat-mode)
                   (and (get-buffer-process (current-buffer))
                        (not (process-running-child-p (get-buffer-process (current-buffer))))))))

;; vterm
(use-package vterm
  :commands (vterm)
  :bind (:map vterm-mode-map
              ("C-c C-t" . vterm-copy-mode))
  :custom
  (vterm-always-compile-module t)
  :hook (vterm-mode . goto-address-mode)
  :config

  ;; Integration with desktop-save-mode
  (defvar vterm-persist-buffer-contents t
    "When t, desktop-save-mode also saves the buffer contents.")
  (defun vterm-save-desktop-buffer (dirname)
    (cons
     (desktop-file-name default-directory dirname)
     (if vterm-persist-buffer-contents (buffer-string) "")))
  (defun vterm-restore-desktop-buffer (_filename buffer-name misc)
    "MISC is the saved return value of `desktop-save-vterm'."
    (let ((default-directory (car misc)))
      (require 'vterm)
      (with-current-buffer (get-buffer-create buffer-name)
        (when vterm-persist-buffer-contents
          (insert (cdr misc))
          (insert "\n\n"))
        (vterm-mode))))
  (add-to-list 'desktop-buffer-mode-handlers '(vterm-mode . vterm-restore-desktop-buffer))
  (add-hook 'vterm-mode-hook #'(lambda () (setq-local desktop-save-buffer 'vterm-save-desktop-buffer))))

;; Eat
(use-package eat
  :bind
  (("M-g v" . eat)  ;; Take place of vterm-toggle
   :map eat-semi-char-mode-map
        ("M-o" . other-window))
  :config
  (setq eat-kill-buffer-on-exit t)
  (setq eat-enable-blinking-text t)
  ;; Note: this integration only works if $TERM starts with "eat".
  ;; So you cannot set $TERM to xterm.
  (setq eat-enable-directory-tracking t))

;; VTerm
(use-package vterm
  :disabled
  :commands (vterm)
  :bind (:map vterm-mode-map
              ("C-c C-t" . vterm-copy-mode)
              ("M-!" . shell-command))
  :custom
  (vterm-always-compile-module t)
  :hook (vterm-mode . goto-address-mode)
  :config

  ;; Integration with desktop-save-mode
  (defvar vterm-persist-buffer-contents t
    "When t, desktop-save-mode also saves the buffer contents.")
  (defun vterm-save-desktop-buffer (dirname)
    (cons
     (desktop-file-name default-directory dirname)
     (if vterm-persist-buffer-contents (buffer-string) "")))
  (defun vterm-restore-desktop-buffer (_filename buffer-name misc)
    "MISC is the saved return value of `desktop-save-vterm'."
    (let ((default-directory (car misc)))
      (require 'vterm)
      (with-current-buffer (get-buffer-create buffer-name)
        (when vterm-persist-buffer-contents
          (insert (cdr misc))
          (insert "\n\n"))
        (vterm-mode))))
  (add-to-list 'desktop-buffer-mode-handlers '(vterm-mode . vterm-restore-desktop-buffer))
  (add-hook 'vterm-mode-hook #'(lambda () (setq-local desktop-save-buffer 'vterm-save-desktop-buffer))))

;; (use-package vterm-toggle
;;   :bind ("M-g v" . vterm-toggle))

;; Dropdown terminal
(defun drop-down-term ()
  "Open a drop-down terminal in the same directory as the current file."
  (interactive)
  (let ((buffer (get-buffer-create "*dd-term*"))
        win)
    (with-current-buffer buffer
      (unless (eq major-mode 'eat-mode)
        (eat-mode))
      (unless (and eat-terminal
                   (eat-term-parameter eat-terminal 'eat--process))
        (eat-exec buffer (buffer-name) "/usr/bin/env" nil
                  (list "sh" "-c" shell-file-name))))
    (setq win
          (display-buffer-in-side-window
           buffer
           '((side . top)
             (dedicated . t))))
    (select-window win)))

(defalias 'dd-term 'drop-down-term)
(defalias 'ddt 'drop-down-term)

(provide 'prelude-term)
;;; prelude-term.el ends here


;;; In the past I tried to parametrize my choice of terminal. But
;;; different terminal packages behave too differently. I think I will
;;; just stick to vterm or eat.

;; ;; Allow setting preferred terminal emulator.
;; ;; Think 'pterm' as (1) preferred term, (2) prelude-term, or (3) parametric term.
;; (defcustom prelude-term 'vterm
;;   "Preferred terminal emulator. Used by project."
;;   :group 'prelude
;;   :type 'symbol)

;; (defun pterm (&rest args)
;;   (interactive)
;;   (cond ((eq prelude-term 'eat)
;;          (funcall 'eat nil args))
;;         (t
;;          (funcall 'vterm args))))

;; (defun pterm-other-window (&rest args)
;;   (interactive)
;;   (cond ((eq prelude-term 'eat)
;;          (funcall 'eat-other-window nil args))
;;         (t
;;          (funcall 'vterm-other-window args))))

;; (defun pterm-run-in-current-buffer ()
;;   (interactive)
;;   (cond ((eq prelude-term 'eat)
;;          (progn
;;            (unless (derived-mode-p 'eat-mode)
;;              (let ((program (or explicit-shell-file-name
;;                               (getenv "ESHELL")
;;                               shell-file-name)))
;;                (eat-mode)
;;                (eat-exec (current-buffer) (buffer-name) "/usr/bin/env" nil
;;                          (list "sh" "-c" program))))))
;;         (t
;;          (unless (derived-mode-p 'vterm-mode)
;;            (vterm-mode)))))
