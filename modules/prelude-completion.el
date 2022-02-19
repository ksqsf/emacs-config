;;; -*- lexical-binding: t; -*-

(defcustom prelude-completion-framework
  'ivy+counsel
  "The completion framework for narrowing selections."
  :group 'prelude
  :type '(choice (const :tag "Icomplete+Ido" icomplete+ido)
                 (const :tag "Ido-only" ido)
                 (const :tag "Ivy+Counsel" ivy+counsel)
                 (const :tag "Selectrum" selectrum)))

(use-package flx)

(cond
 ;; Ivy + counsel
 ((eq prelude-completion-framework 'ivy+counsel)

  (use-package ivy
    :diminish "ⓘ"
    :hook (after-init . ivy-mode)
    :config
    (setq ivy-use-virtual-buffers 'recentf)
    (setq ivy-height 10)
    (setq ivy-fixed-height-minibuffer t)

    (define-key ivy-occur-mode-map (kbd "p") 'ivy-occur-previous-line)
    (define-key ivy-occur-mode-map (kbd "n") 'ivy-occur-next-line)
    (define-key ivy-occur-mode-map (kbd "f") 'forward-char)
    (define-key ivy-occur-mode-map (kbd "b") 'backward-char))

  (use-package ivy-hydra
    :after ivy)

  (use-package ivy-prescient
    :hook ((ivy-mode . ivy-prescient-mode)
           (ivy-prescient-mode . prescient-persist-mode)))

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
    :diminish "ⓒ"
    :hook (ivy-mode . counsel-mode)
    :bind (("M-s M-s" . swiper)
           ;; ("C-r" . swiper-isearch-backward)
           ("C-c C-r" . ivy-resume)
           ("C-c v p" . ivy-push-view)
           ("C-c v o" . ivy-pop-view)
           ("C-c v ." . ivy-switch-view))))

 ;; Selectrum
 ((eq prelude-completion-framework 'selectrum)
  (use-package selectrum
    :hook (after-init . selectrum-mode)
    :config
    (use-package selectrum-prescient
      :config
      (selectrum-prescient-mode +1)
      (prescient-persist-mode +1))))

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
