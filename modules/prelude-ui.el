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

;; NOTE: {initial,default}-buffer-alist is moved to early-init.el

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
              ("C-x C-," . winner-undo)
              ("C-x C-." . winner-redo))
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
(use-package all-the-icons :demand t)

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

;; let left and right side windows occupy full frame height
(setopt window-sides-vertical t)

;; fix bad default behavior
;; ... but some packages (e.g. checkdoc) rely on the default behavior.
;; (setopt display-buffer-base-action
;;         '((display-buffer-reuse-window display-buffer-same-window)
;;           (reusable-frames . t)))
(setopt even-window-sizes nil)
(setq window-combination-limit 'display-buffer)
(setq window-combination-resize t)

;; Helper function for display-buffer-alist.
(defun +has-mode (major-modes)
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

;; I hate emacs layout venomously!
;; Let's fix it once and for all.
;; FIXME: This still does not do what I want.
;; I want a "side" to be a list of buffers associated with this "side".
;; And I can hide or show them at once.
;; The "side window" however is still disconnected with the buffers.
(setopt display-buffer-alist
 `(;; Side windows: bottom
   (,(+has-mode '(magit-status-mode))
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side . bottom)
    (slot . 0)
    (window-height . 0.4)
    (dedicated . t))
   ("^\\*.*-?compilation\\*"
    (display-buffer-reuse-window
     display-buffer-pop-up-window
     display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.3)
    (slot . 1)
    (dedicated . t))
   ("^\\*Backtrace\\*\\'"
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.3)
    (slot . 2)
    (dedicated . t))
   ("\\*quickrun\\*"
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side . bottom)
    (slot . 3)
    (dedicated . t))
   (,(+has-mode '(haskell-interactive-mode))
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.4)
    (slot . 4)
    (dedicated . t))
   (,(+has-mode '(inferior-python-mode))
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (window-height . 0.4)
    (side . bottom)
    (slot . 5)
    (dedicated . t))
   ("^\\*\\(.*-\\)?e?shell\\*"
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (side . bottom)
    (slot . 6)
    (window-height . 0.4)
    (dedicated . t)
    (dedicated . t))
   ("^\\*vterm"
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (window-height . 0.4)
    (mode vterm-mode vterm-copy-mode)
    (side . bottom)
    (slot . 7)
    (dedicated . t)
    (dedicated . t))
   ("^\\*ielm\\*"
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.4)
    (slot . 8)
    (dedicated . t))
   (,(+has-mode '(comint-mode))
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.4)
    (slot . 9)
    (dedicated . t))
   ("^\\*[Ff]ly"
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (side . bottom)
    (window-height . 0.4)
    (slot . 10)
    (dedicated . t))

   ;; Side windows: right
   ("^\\*[Hh]elp.*"
    (display-buffer-reuse-mode-window
     display-buffer-in-side-window)
    (side . right)
    (window-width . 80)
    (slot . 0)
    (dedicated . t)
    (inhibit-same-window . t))

   ;; Temporary buffers that contain rich contents
   ("^\\*ripgrep-search\\*"
    (display-buffer-reuse-window
     display-buffer-reuse-mode-window
     display-buffer-at-bottom))
   ("^\\*grep\\*"
    (display-buffer-reuse-window
     display-buffer-reuse-mode-window
     display-buffer-at-bottom))
   ("^\\*vc-diff\\*\\'"
    (display-buffer-reuse-window
     display-buffer-reuse-mode-window
     display-buffer-in-direction)
    (direction . right))
   ("^\\*Warnings"
    (display-buffer-reuse-window
     display-buffer-at-bottom))

   ;; Telega
   (,(+has-mode '(telega-root-mode))
    (display-buffer-in-side-window)
    (window-width . 0.35)
    (side . right)
    (slot . 0))
   (,(+has-mode '(telega-chat-mode))
    (display-buffer-reuse-mode-window
     display-buffer-at-bottom)
    (mode . telega-chat-mode)
    (dedicated . t))

   ;; Fix magit commit: show the two buffers simultaneously.
   ("COMMIT_MSG"
    (display-buffer-in-side-window)
    (side . bottom)
    (slot . 0))
   ("^magit-diff"
    (display-buffer-in-side-window)
    (side . bottom)
    (slot . 1))
   ))

;; FIXME: window-toggle-side-windows unfortunately relies on the
;; existence of side windows to decide whether to show or to hide side
;; windows.  It cannot toggle only one side.  Treemacs is a victim.
(global-set-key [C-tab] #'window-toggle-side-windows)

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
(with-eval-after-load 'hideshow
  (diminish 'hs-minor-mode ""))

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
    (push i mixed-pitch-fixed-pitch-faces)))

;; I don't really like which-key, but it's helpful when it's
;; helpful...  *wink*
(use-package which-key
  :diminish ""
  :hook (after-init . which-key-mode))

;; Preferred dark and light themes.
(use-package one-themes :defer t)
(use-package catppuccin-theme :defer t)
(use-package dracula-theme :defer t)
(use-package doom-themes :defer t)

(defcustom prelude-enable-switch-dark-light nil
  "Whether automatically switch the current theme to match the
system's dark or light variant."
  :group 'prelude
  :type 'boolean)
(defcustom prelude-theme-package 'doom-themes
  "The package that defines `prelude-dark-theme' and `prelude-light-theme'."
  :group 'prelude
  :type 'symbol)
(defcustom prelude-dark-theme 'doom-oksolar-dark
  "Preferred dark theme."
  :group 'prelude
  :type 'symbol)
(defcustom prelude-light-theme 'doom-oksolar-light
  "Preferred light theme."
  :group 'prelude
  :type 'symbol)

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
      (load-theme prelude-dark-theme))
     ((eq appearance 'light)
      (disable-theme prelude-dark-theme)
      (load-theme prelude-light-theme)))))

(add-hook 'ns-system-appearance-change-functions #'prelude-switch-light-dark)

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

;; FIXME: Side windows?
(defun window-lift ()
  "Lift the selected window to replace its parent in the window tree."
  (interactive)
  (let ((sibling (or (window-prev-sibling)
                     (window-next-sibling)))
        (keymap (let ((keymap (make-sparse-keymap)))
                  (define-key keymap (kbd "l") #'window-lift)
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

(global-set-key (kbd "C-x l") #'window-lift)

(use-package dashboard
  :commands (dashboard-refresh-buffer)
  :config
  (setq dashboard-items '((recents . 5)
                          (projects . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-banner-logo-title "Happy Hacking!"))

(use-package tab-bar
  :disabled
  :ensure nil
  :demand t
  :custom
  (tab-bar-mode t)
  (tab-bar-history-mode t)
  :config
  (setq tab-bar-new-tab-choice #'dashboard-refresh-buffer)

  ;; Make tab-bar--load-buttons do nothing to prevent it from
  ;; overriding our pretty icons.
  (advice-add 'tab-bar--load-buttons :override (lambda (&rest _args)))

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


;; line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-grow-only t
              display-line-numbers-width 4
              line-number-extra-spaces 2  ; This depends on one of my custom patches
              )


;; toggle mwheel horizontal scroll when toggling truncate-lines
(advice-add 'toggle-truncate-lines :after '+enable-horizontal-scroll)
(defun +enable-horizontal-scroll (&rest _)
  (if truncate-lines
      (progn (setq mouse-wheel-tilt-scroll t)
             (setq mouse-wheel-flip-direction t))
    (setq mouse-wheel-tilt-scroll nil)))


(use-package page-break-lines
  :disabled
  :hook (prog-mode . page-break-lines-mode))


(defun set-frame-alpha (alpha)
  (interactive "sAlpha: ")
  (set-frame-parameter nil 'alpha (string-to-number alpha)))

(defun toggle-frame-decorated ()
  (interactive)
  (let ((v (frame-parameter nil 'undecorated)))
    (set-frame-parameter nil 'undecorated (not v))))


(use-package rotate
  :defer t)


(use-package vterm-toggle
  :bind ("M-g v" . vterm-toggle))

(provide 'prelude-ui)
