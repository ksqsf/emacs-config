;;; -*- lexical-binding: t; -*-

(defcustom prelude-ui-accent-color
  "brown2"
  "Accent color of UI elements. This affects the mode line."
  :group 'prelude
  :type '(color))

(defcustom prelude-ui-frame-alpha       ; TODO
  0.92
  "Default frame alpha."
  :group 'prelude)

;; Pixelwise resize
(setq frame-resize-pixelwise t)

;; A quick way to toggle maximized
(global-set-key (kbd "C-M-<return>") #'toggle-frame-maximized)

;; Don't show useless UI elements
(add-hook 'after-init-hook
          #'(lambda ()
              (menu-bar-mode 1)
              (tool-bar-mode 0)))

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
(add-to-list 'initial-frame-alist '(height . 50))
(add-to-list 'initial-frame-alist '(width . 140))

;; Mac-specific settings
(when *is-a-mac*
  (use-package ns-auto-titlebar
    :init
    (ns-auto-titlebar-mode)))

;; Default theme
(use-package lab-themes
  :disabled
  :config
  (defun prelude-switch-theme (appearance)
    (if (eq appearance 'dark)
        (load-theme 'lab-dark t)
      (load-theme 'lab-light t)))
  (add-hook 'ns-ns-system-appearance-change-functions #'prelude-switch-theme))

;; (use-package bespoke-themes
;;   :quelpa (bespoke-themes :fetcher github :repo "mclear-tools/bespoke-themes")
;;   (bespoke-themes-visual-bell-config)
;;   (load-theme 'bespoke-light t))

;; Mode line
(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode))
  :init
  (setq doom-modeline-minor-modes nil)
  :config
  (set-face-attribute 'doom-modeline-bar nil :background prelude-ui-accent-color))

;; I'm the winner ;-)
(use-package winner
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

;; Auto set margins
(use-package perfect-margin
  :disabled
  :quelpa (perfect-margin :fetcher github :repo "mpwang/perfect-margin")
  :custom
  (perfect-margin-visible-width 128)
  :config
  (setq perfect-margin-ignore-regexps '("^minibuf"))
  (perfect-margin-mode t)
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll))))

;; Dashboard
(use-package dashboard
  :disabled
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((projects . 5)
                          (bookmarks . 5)
                          (recents . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

;; Cycle themes
(defcustom prelude-themes
  (list 'doom-monokai-pro) 
  "Themes that you may want to use."
  :group 'prelude)

(defun cycle-theme ()
  "Cycle around `prelude-themes'."
  (interactive)
  (require 'dash)
  (if (> (length prelude-themes) 1)
      (let ((next (car prelude-themes)))
        (dolist (current custom-enabled-themes)
          (disable-theme current))
        (if (listp next)
            (dolist (theme next)
              (load-theme theme t))
          (load-theme next t))
        (setq prelude-themes (append (cdr prelude-themes)
                                     (list (car prelude-themes)))))))

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
  (global-ligature-mode t))

;; prioritize vertical side windows
(setq window-sides-vertical t)


;; helper
(defmacro buffer-is-major-mode (major-mode)
  `(lambda (buffer alist)
     (with-current-buffer buffer
       (message "in buffer %s: %s" buffer major-mode)
       (eql major-mode ,major-mode)))) ;; 'eq' does not work

;; popups policy
(setq
 display-buffer-alist
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
    (window-height . 0.5)
    (window-width . 72)
    (slot . 0)
    (dedicated . t))

   ;; Bottom Side
   ("^\\*compilation"
    (display-buffer-reuse-window
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
    (slot . 1)
    (dedicated . t))
   (,(buffer-is-major-mode 'calc-mode)
    (display-buffer-in-side-window)
    (side . bottom)
    (slot . 1)
    (dedicated . t))

   ;; Below Selected
   ("^\\*\\(\\(e?shell\\)\\|\\(vterm\\)\\)"
    (display-buffer-below-selected)
    (window-width . 72))
   (,(buffer-is-major-mode 'haskell-interactive-mode)
    (display-buffer-reuse-window
     display-buffer-below-selected))
   (,(buffer-is-major-mode 'inferior-python-mode)
    (display-buffer-below-selected))

   ;; Terminals
   ("^\\*\\(.*-\\)?e?shell\\*"
    nil
    (inhibit-same-window . t)
    (dedicated . t))
   ("^\\*\\(.*-\\)?vterm\\*"
    nil
    (inhibit-same-window . t)
    (dedicated . t))
   ))

(use-package minimap
  :disabled
  :config
  (setq minimap-window-location 'right))

(provide 'prelude-ui)
