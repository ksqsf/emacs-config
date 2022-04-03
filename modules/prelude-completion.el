;;; -*- lexical-binding: t; -*-

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package marginalia
  :after (selectrum)
  :config
  (marginalia-mode))

(use-package embark
  :bind
  (("C-," . embark-act)
   ("C-." . embark-dwim)
   ("C-h B" . embark-bindings))
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
   ("M-s m" . consult-multi-occur)
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

  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-narrow-key "<"))

(use-package embark-consult
  :after (embark consult)
  :demand t
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
  (setq completion-styles '(basic partial-completion flex))
  (setq completion-category-defaults nil)
  (setq completions-format 'vertical)
  (setq-default completion-at-point-functions ))

(provide 'prelude-completion)
