;;; -*- lexical-binding: t; -*-

;; Pixelwise resize
(setq frame-resize-pixelwise t)

;; A quick way to toggle maximized
(global-set-key (kbd "C-M-<return>") #'toggle-frame-maximized)

;; A quick way to toggle side windows
(global-set-key (kbd "C-z") #'window-toggle-side-windows)

;; Enable smooth scroll when it's available
;; ... but it's still buggy.
;; (when (fboundp 'pixel-scroll-precision-mode)
;;   (pixel-scroll-precision-mode t))

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
(when k|mac
  (use-package ns-auto-titlebar
    :init
    (ns-auto-titlebar-mode)))

;; Mode line
(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode))
  :init
  (setq doom-modeline-minor-modes nil))

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
   (,(k|buffer-is-major-mode 'calc-mode)
    (display-buffer-in-side-window)
    (side . bottom)
    (slot . 1)
    (dedicated . t))
   ("^HELM .*"
    (display-buffer-at-bottom))

   ;; Below Selected
   ("^\\*\\(\\(e?shell\\)\\|\\(vterm\\)\\)"
    (display-buffer-below-selected)
    (window-width . 72))
   (,(k|buffer-is-major-mode 'haskell-interactive-mode)
    (display-buffer-reuse-window
     display-buffer-at-bottom
     display-buffer-below-selected))
   (,(k|buffer-is-major-mode 'inferior-python-mode)
    (display-buffer-below-selected))

   ;; Terminals
   ("^\\*\\(.*-\\)?e?shell\\*"
    nil
    (inhibit-same-window . t)
    (dedicated . t))
   ("^\\*\\(.*-\\)?vterm\\*"
    nil
    (inhibit-same-window . t)
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
  (use-package vterm)
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
  :bind (("C-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
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

;; Provide mixed-pitch faces
(use-package mixed-pitch
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

(provide 'prelude-ui)
