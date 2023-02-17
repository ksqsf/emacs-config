;;; -*- lexical-binding: t; -*-

;; Frame resize
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

;; A quick way to toggle maximized
(global-set-key (kbd "C-M-<return>") #'toggle-frame-fullscreen)

;; Enable smooth scroll when it's available
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; Don't show useless UI elements
(menu-bar-mode 1)
(tool-bar-mode -1)

;; My vendor settings!
;; 1. Choose a better background color (thanks to Xah Lee)
;; 2. Better default size
;;
;; I strongly advise you write equivalent settings in your ~/.Xresources!
(setq default-frame-alist nil)
(setq initial-frame-alist nil)
(if (display-graphic-p)
    (progn
      (setq default-frame-alist
            '((vertical-scroll-bars . nil))))
  (progn
    (setq default-frame-alist '((tool-bar-lines . 0)))))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 120))

;; Mac-specific settings
(when k|mac
  (use-package ns-auto-titlebar
    :init
    (ns-auto-titlebar-mode)))

;; Mode line
(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode))
  :init
  (setq doom-modeline-minor-modes nil)

  ;; projectile undesirably resolves symlinks. This is a workaround.
  (when k|mac
    (setq doom-modeline-project-detection 'project)))

(use-package moody
  :disabled
  :config
  (defun prelude-activate-moody (&optional disactivate)
    "If DISACTIVATE is t, moody is disabled."
    (setq arg (or disactivate nil))
    (moody-replace-mode-line-buffer-identification arg)
    (moody-replace-vc-mode arg)
    (moody-replace-eldoc-minibuffer-message-function arg))
  (setq x-underline-at-descent-line t)
  (prelude-activate-moody t))

;; I'm the winner ;-)
(use-package winner
  :ensure nil
  :defer t
  :bind (:map winner-mode-map
              ("C-c C-," . winner-undo)
              ("C-c C-." . winner-redo))
  :init
  (defvar winner-dont-bind-my-keys t)
  (add-hook 'after-init-hook #'winner-mode))

(use-package winum
  ;; M-0 is reserved for Treemacs
  ;; Treemacs will be moved here from apps soon
  :hook ((after-init . winum-mode))
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9)))

;; Eye candy icons
(use-package all-the-icons
  :defer t)

;; Rainbow
(use-package rainbow-mode
  :commands (rainbow-mode))

;; Enable ligatures
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

;; valign
(use-package valign
  :commands (valign-mode))

;; ligatures
(use-package ligature
  :quelpa (ligature :fetcher github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://" "/\\" "\\/"))
  (ligature-set-ligatures 'markdown-mode '("[ ]" "[X]"))
  (global-ligature-mode t))

;; prioritize vertical side windows
(setq window-sides-vertical t)

;; helper
(defmacro k|buffer-is-major-mode (major-mode)
  `(lambda (buffer alist)
     (with-current-buffer buffer
       (eql major-mode ,major-mode)))) ;; 'eq' does not work

;; popups policy
(setq display-buffer-alist
 `(;; Bottom Root
   ("^\\*scratch\\*\\'"
    (display-buffer-reuse-window
     display-buffer-at-bottom
     display-buffer-same-window))
   ("^magit: .*"
    (display-buffer-reuse-window
     display-buffer-at-bottom)
    (window-height . 0.4)
    (dedicated . t))

   ;; Right Side
   ("^\\*[Hh]elp"
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side . right)
    (window-height . 0.4)
    (window-width . 80)
    (slot . 0)
    (dedicated . t))

   ;; Bottom Side
   ("^\\*compilation"
    (display-buffer-reuse-window
     display-buffer-pop-up-window)
    ;; (side . bottom)
    ;; (window-height . 0.3)
    ;; (slot . 1)
    (dedicated . t))
   ("^\\*Backtrace\\*\\'"
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.3)
    (slot . 1)
    (dedicated . t))
   (,(k|buffer-is-major-mode 'calc-mode)
    (display-buffer-in-side-window)
    (side . bottom)
    (slot . 1)
    (dedicated . t))
   ("^HELM .*"
    (display-buffer-at-bottom))
   ("\\*[Hh]elm.*"
    (display-buffer-at-bottom))
   ("\\*quickrun\\*"
    (display-buffer-reuse-window
     display-buffer-pop-up-window)
    nil)

   ;; Below Selected
   ;; ("^\\*\\(\\(e?shell\\)\\|\\(vterm\\)\\)"
   ;;  (display-buffer-below-selected)
   ;;  (window-width . 72))
   (,(k|buffer-is-major-mode 'haskell-interactive-mode)
    (display-buffer-reuse-window
     display-buffer-at-bottom
     display-buffer-below-selected))
   (,(k|buffer-is-major-mode 'inferior-python-mode)
    (display-buffer-below-selected))

   ;; Terminals
   ("^\\*\\(.*-\\)?e?shell\\*"
    (display-buffer-reuse-window
     display-buffer-below-selected)
    (inhibit-same-window . nil)
    (dedicated . t))
   ("^\\*vterm"
    (display-buffer-reuse-window
     display-buffer-below-selected)
    (window-height . 0.37)
    (inhibit-same-window . nil)
    (dedicated . t))))

;; Minimap
(use-package minimap
  :commands (minimap-mode)
  :config
  (setq minimap-window-location 'right))

;; Dropdown terminal
(defun drop-down-term ()
  "Open a drop-down terminal in the same directory as the current file."
  (interactive)
  (use-package vterm :ensure t)
  (let ((buffer (get-buffer-create "*dd-term*"))
        win)
    (with-current-buffer buffer
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode)))
    (setq win
          (display-buffer-in-side-window
           buffer
           '((side . top)
             (dedicated . t))))
    (select-window win)))

(defalias 'dd-term 'drop-down-term)

;; Pop-up windows
(use-package popper
  :disabled
  :defer 1
  :bind (("C-`" . popper-toggle-latest)
         ;; ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          interactive-haskell-mode))
  (setq popper-group-function nil)
  (popper-mode +1)
  (popper-echo-mode +1))

;; Enable context menu
(context-menu-mode +1)
(setq-default context-menu-functions
              '(context-menu-ffap
                k|context-menu-hideshow
                occur-context-menu
                context-menu-region
                context-menu-undo))

(defun k|context-menu-hideshow (menu click)
  "Populate MENU with `hideshow' commands."
  (save-excursion
    (mouse-set-point click)
    (if (hs-already-hidden-p)
        (define-key-after menu [hs-show-block]
          '(menu-item "Show block"
                      (lambda (click) (interactive "e")
                        (save-excursion
                          (mouse-set-point click)
                          (hs-show-block)))))
      (define-key-after menu [hs-hide-block]
        '(menu-item "Hide block"
                    (lambda (click) (interactive "e")
                      (save-excursion
                        (mouse-set-point click)
                        (hs-hide-block)))))))
  (define-key-after menu [hs-separator]
    menu-bar-separator)
  menu)

;; Hideshow
(add-hook 'prog-mode-hook #'hs-minor-mode)
(diminish 'hs-minor-mode "")

;; Provide mixed-pitch faces
(use-package mixed-pitch
  :disabled
  :hook
  (org-mode . mixed-pitch-mode)
  (markdown-mode . mixed-pitch-mode)
  ;; We can't simply use text-mode here: Magit COMMIT_MSG is also
  ;; text-mode.
  :config
  (dolist (i '(org-drawer org-special-keyword org-property-value))
    (push i mixed-pitch-fixed-pitch-faces))
  (set-face-attribute 'variable-pitch nil :font "Arial"))

;; I don't really like which-key, but it's helpful when it's
;; helpful...  *wink*
(use-package which-key
  :diminish ""
  :hook (after-init . which-key-mode))

;; `delete-other-windows' will signal an error when the current window
;; is a side window.  Hack it so it display the current buffer in a
;; main window.
(defun k|window-is-side (&optional win)
  "Returns non-nil iff WIN is a side window.

WIN means the current window if it is nil."
  (window-parameter win 'window-side))

(defun k|first-main-window ()
  "Returns the first window that is not a side window.

The existence of such windows is guaranteed by Emacs."
  ;; dash.el is much better:
  ;; (--first (not (k|window-is-side win)) (window-list))
  (cl-loop for win in (window-list)
           if (not (k|window-is-side win))
           return win))

(defun k|delete-other-windows ()
  "`delete-other-windows' that supports side windows."
  (interactive)
  (if (k|window-is-side)
      (let ((buf (current-buffer))
            (win (k|first-main-window)))
        (delete-other-windows win)
        (switch-to-buffer buf))
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'k|delete-other-windows)

;; Custom fonts
(custom-set-faces
 ;; '(default ((t (:family "Hasklig" :height 140))))
 '(variable-pitch ((t (:family "Bookerly" :height 1.0))))
 ;; '(treemacs-file-face ((t (:inherit variable-pitch))))
 ;; '(treemacs-root-face ((t (:inherit variable-pitch))))
 ;; '(treemacs-directory-face ((t (:inherit variable-pitch))))
 ;; '(treemacs-directory-collapsed-face ((t (:inherit variable-pitch))))
 ;; '(treemacs-git-ignored-face ((t (:inherit variable-pitch))))
 )

;; Preferred dark and light themes.
(defcustom prelude-enable-switch-dark-light nil
  "Whether automatically switch the current theme to match the
system's dark or light variant."
  :group 'prelude)
(defcustom prelude-theme-package 'one-themes
  "The package that defines `prelude-dark-theme' and `prelude-light-theme'."
  :group 'prelude)
(defcustom prelude-dark-theme 'one-dark
  "Preferred dark theme."
  :group 'prelude)
(defcustom prelude-light-theme 'one-light
  "Preferred light theme."
  :group 'prelude)

(defun prelude-switch-light-dark (appearance)
  (catch 'foo
    (when (not prelude-enable-switch-dark-light)
      (throw 'foo nil))
    (when (not (package-installed-p prelude-theme-package))
      (package-install prelude-theme-package))
    (when (not (featurep prelude-theme-package))
      (require prelude-theme-package))
    (cond
     ((eq appearance 'dark)
      (disable-theme prelude-light-theme)
      (enable-theme prelude-dark-theme))
     ((eq appearance 'light)
      (disable-theme prelude-dark-theme)
      (enable-theme prelude-light-theme)))))

(add-hook 'ns-system-appearance-change-functions #'prelude-switch-light-dark)

;; vterm
(use-package vterm
  :commands (vterm)
  :bind (:map vterm-mode-map
              ("C-c C-x" . vterm-send-C-x)
              ("C-c C-t" . vterm-copy-mode))
  :custom
  (vterm-always-compile-module t)
  :config
  (define-key vterm-mode-map (kbd "C-c C-x") #'vterm-send-C-x)
  (add-hook 'vterm-mode-hook 'goto-address-mode))

;; FIXME: Side windows?
(defun window-lift ()
  "Lift the selected window to replace its parent in the window tree."
  (interactive)
  (let ((sibling (or (window-prev-sibling)
                     (window-next-sibling)))
        (keymap (let ((keymap (make-sparse-keymap)))
                  (define-key keymap (kbd "1") #'window-lift)
                  keymap)))
    ;; In the following configuration
    ;;
    ;; |-----|-----|
    ;; | W1  |     |
    ;; |-----| W3  |
    ;; | W2  |     |
    ;; |-----|-----|
    ;;
    ;; If W3 is selected, it's prev-sibling won't be a leaf.
    (when (and sibling
               (not (and (window-buffer sibling)
                         (minibufferp (window-buffer sibling)))))
      (set-transient-map keymap t nil nil 0.5)
      (delete-window sibling))))

;; (global-set-key (kbd "C-x 1") #'window-lift)  ;; Not ready yet.

(use-package dashboard
  :config
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          (registers . 2)))
  (setq dashboard-center-content t)
  (setq dashboard-banner-logo-title "Happy Hacking!"))

(use-package tab-bar
  :ensure nil
  :after (all-the-icons)
  :hook (after-init . tab-bar-mode)
  :hook (after-init . tab-bar-history-mode)
  :config
  (setq tab-bar-new-tab-choice #'dashboard-refresh-buffer)

  ;; the low-res icons are soooo ugly!!
  ;; the proper way to do this is perhaps defining tab-bar-* icons before loading tab-bar
  ;; but all-the-icons has another definition for 'define-icon'.
  ;; so let's do it this way until all-the-icons remove that definition.
  (setq tab-bar-new-button (all-the-icons-material "add")
        tab-bar-back-button (all-the-icons-faicon "chevron-left")
        tab-bar-forward-button (all-the-icons-faicon "chevron-right"))
  ;; we can't use all-the-icons for the close button because its face
  ;; gets overwritten by tab-bar's format function.
  (setq tab-bar-close-button-show t)
  (setq tab-bar-close-button (propertize " ⨉" 'close-tab t))
  (setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-add-tab)))

(use-package hl-line
  :hook (after-init . global-hl-line-mode))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

;;

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative
      display-line-numbers-grow-only t
      display-line-numbers-width 4)

(provide 'prelude-ui)
