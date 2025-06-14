;;; -*- lexical-binding: t; -*-

(use-package completion
  :ensure nil
  :custom
  (completion-styles '(prescient orderless)))

(use-package prescient
  :demand t
  :config
  (prescient-persist-mode))

(use-package orderless
  :demand t
  :custom
  (orderless-matching-styles
   '(orderless-prefixes                 ; Match a component as multiple word prefixes.
     orderless-initialism               ; Match a component as an initialism.
     orderless-flex                     ; Match a component in flex style.
     orderless-regexp))                 ; Match a component as a regexp.
  (orderless-smart-case t))

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (vertico-mouse-mode +1)

  ;; Configure how vertico is displayed on a per-category or
  ;; per-command basis.
  (setq vertico-multiform-commands
        '((consult-imenu buffer indexed)
          (consult-line buffer)
          (consult-line-multi buffer)))
  (setq vertico-multiform-categories
        '((consult-grep buffer)))
  (vertico-multiform-mode t)

  ;; Emacs requires the point to be always in the view.  When the
  ;; point is near the modeline, pressing e.g. M-x will cause a
  ;; recenter, which is annoying.
  (defun +move-point-to-the-beginning-of-the-window ()
    (dolist (win (window-list))
      (let ((buf (window-buffer)))
        (with-current-buffer buf
          (unless (minibufferp)
            (goto-char (window-start win))
            (push-mark))))))
  (defun +restore-saved-point ()
    (dolist (win (window-list))
      (let ((buf (window-buffer win)))
        (unless (minibufferp)
          (with-current-buffer buf
          (pop-mark))))))
  ;; Unfortunately, this does not work yet.
  ;; (add-hook 'minibuffer-setup-hook #'+move-point-to-the-beginning-of-the-window)
  ;; (add-hook 'minibuffer-exit-hook #'+restore-saved-point)

  ;; vertico-repeat
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (keymap-set vertico-map "C-r" #'vertico-repeat)
  (keymap-set vertico-map "C-s" #'vertico-repeat-next)
  (with-eval-after-load 'vertico-repeat
    (setq vertico-repeat-filter
          (remove 'execute-extended-command
                  (remove 'execute-extended-command-for-buffer
                          vertico-repeat-filter))))
  )

(use-package vertico-posframe
  :disabled
  :after (vertico)
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (setq vertico-posframe-border-width 3))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package embark
  :bind
  (("C-," . embark-act)
   ("C-." . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-mode-map
   ("C-c C-o" . embark-export)  ;; This is the default binding of Ivy-Occur
   )
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :bind
  ;; ctl-x-map
  (("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   
   ;; goto-map
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   
   ;; search-map
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)

   ;; isearch related
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   )
  
  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  :init

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-narrow-key "<")
  (setq consult-async-min-input 1)

  ;; vertico-posframe overlaps with the matched line.
  (defun k|recenter-around-top ()
    "Equivalent to `recenter-top-bottom' with argument 0.2."
    (recenter (round (* 0.2 (window-height))) t))
  (setq consult-after-jump-hook '(k|recenter-around-top)))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-consult-mode . consult-preview-at-point-mode))

(use-package minibuffer
  :ensure nil
  :config
  (setq read-answer-short t)
  (setq resize-mini-windows 'grow-only)
  (setq enable-recursive-minibuffers t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq completion-category-defaults nil)
  (setq completions-format 'vertical)
  (setq-default completion-at-point-functions nil))

;; Additional capf.
(use-package cape
  :demand t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-preview-current t)
  (corfu-cycle t)
  (corfu-preselect 'valid)
  :bind (:map corfu-map
              ("<return>" . nil)
              ("<escape>" . corfu-quit)
              ("M-d" . corfu-popupinfo-documentation)
              ("M-l" . corfu-info-location))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  ;; Let RET be newlines.
  (define-key corfu-map (kbd "RET") nil))

(use-package nerd-icons-corfu
  :after (corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package kind-icon
  :disabled
  :after (corfu)
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-terminal
  :if (and (not (display-graphic-p))     ; not GUI
           (not (fboundp 'tty-tip-mode)) ; not compiled with tty child frames
           )
  :demand t
  :init
  (corfu-terminal-mode))

(provide 'prelude-completion)
