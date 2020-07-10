;;; -*- lexical-binding: t; -*-

(defcustom prelude-completion-framework
  'ivy+counsel
  "The completion framework for narrowing selections."
  :group 'prelude
  :type '(choice (const :tag "Icomplete+Ido" icomplete+ido)
                 (const :tag "Ido-only" ido)
                 (const :tag "Ivy+Counsel" ivy+counsel)))

(use-package flx)

(cond
 ;; Ivy + counsel
 ((eq prelude-completion-framework 'ivy+counsel)

  (use-package ivy
    :hook (after-init . ivy-mode)
    :config
    (setq ivy-use-virtual-buffers 'recentf)
    (setq ivy-height 10)
    (setq ivy-fixed-height-minibuffer t))

  (use-package ivy-hydra
    :after ivy)

  (use-package ivy-prescient
    :after ivy)

  (use-package ivy-rich
    :after ivy
    :hook (ivy-mode . ivy-rich-mode)
    :config
    (setq ivy-rich-parse-remote-buffer nil)
    (use-package all-the-icons-ivy-rich
      :config
      (setq all-the-icons-ivy-rich-icon-size 0.80)
      (all-the-icons-ivy-rich-mode 1)))

  (use-package ivy-posframe
    :disabled                           ; Doesn't work well with ivy-rich
    :after ivy
    :hook (ivy-mode . ivy-posframe-mode)
    :config
    (setq ivy-posframe-parameters
          '((left-fringe . 8)
            (right-fringe . 8)))
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display))))

  (use-package counsel
    :hook (ivy-mode . counsel-mode)
    :bind (;; ("C-s" . swiper-isearch)
           ;; ("C-r" . swiper-isearch-backward)
           
           ("C-c C-r" . ivy-resume)
           ("C-c v p" . ivy-push-view)
           ("C-c v o" . ivy-pop-view)
           ("C-c v ." . ivy-switch-view))
    :config
    (use-package amx)))

 ;; Icomplete + Ido
 ((eq prelude-completion-framework 'icomplete+ido)
  (setq ido-use-virtual-buffers t)
  (ido-mode 1)
  (setq icomplete-max-delay-chars 0
        icomplete-in-buffer t
        icomplete-show-matches-on-no-input t
        icomplete-delay-completions-threshold 30000)
  (icomplete-mode 1))

 ;; Ido
 ((eq prelude-completion-framework 'ido)
  (use-package flx-ido
    :after ido
    :config
    (flx-ido-mode 1))
  (use-package ido-grid-mode
    :after ido)
  (use-package ido-completing-read+
    :config
    (ido-ubiquitous-mode 1))
  (use-package amx
    :config
    (setq amx-backend 'ido)
    (amx-mode 1))
  (ido-mode 1)))

(use-package minibuffer
  :ensure nil
  :config
  (setq read-answer-short t)
  (setq resize-mini-windows t)
  (setq enable-recursive-minibuffers t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq completion-styles '(basic partial-completion flex))
  (setq completion-category-defaults nil)
  (setq completions-format 'vertical))

(provide 'prelude-completion)
