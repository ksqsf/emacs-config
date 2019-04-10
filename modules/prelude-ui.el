;; Don't show useless UI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; My vendor settings!
;; 1. Choose a better background color (thanks to Xah Lee)
;; 2. Better default size
;;
;; I strongly advise you write equivalent settings in your ~/.Xresources!
(setq default-frame-alist nil)
(setq initial-frame-alist nil)
(if (display-graphic-p)
    (progn
     (setq initial-frame-alist
	   '((tool-bar-lines . 0)
	     (left-fringe . 8) (right-fringe . 8)
	     (width . 110) (height . 45)
	     (background-color . "honeydew")))
     (setq default-frame-alist
	   '((tool-bar-lines . 0)
	     (width . 110) (height . 45)
	     (left-fringe . 8) (right-fringe . 8)
	     (background-color . "honeydew"))))
  (progn
    (setq initial-frame-alist '((tool-bar-lines . 0)))
    (setq default-frame-alist '((tool-bar-lines . 0)))))

;; Don't "raise" the mode line, this 3D is too ugly!
(set-face-attribute 'mode-line nil :box '(:line-width -1 :color "gray60"))

;; Smex for faster command typing
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c M-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Ido for faster file navigation
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t)

(unless (package-installed-p 'ido-grid-mode)
  (package-install 'ido-grid-mode))
(ido-grid-mode t)

;; I'm the winner ;-)
(defvar winner-dont-bind-my-keys t)
(winner-mode t)
(define-key winner-mode-map (kbd "C-c ,") 'winner-undo)
(define-key winner-mode-map (kbd "C-c .") 'winner-redo)

(provide 'prelude-ui)
