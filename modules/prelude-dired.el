;;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-x C-j") #'dired-jump)

(with-eval-after-load 'dired
  ;; Search file names when point is at a file name; Search unlimitedly
  ;; otherwise.
  (setq dired-isearch-filenames 'dwim)

  ;; Intelligently guess the target directory.
  (setq dired-dwim-target t)

  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" k|default-opener)
          ("\\.png\\'" k|default-opener)
          ("\\.jpg\\'" k|default-opener)))

  (when k|mac
    (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.DS_Store\\|\\.localized")))

(use-package dired-filter
  :after (dired)
  :hook ((dired-mode . dired-filter-group-mode)
         (dired-mode . dired-filter-mode))
  :config
  (define-key dired-mode-map "/" dired-filter-map)
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Git"
            (directory . ".git")
            (file . ".gitignore"))
           ("Directory"
            (directory))
           ("PDF"
            (extension . "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("Code"
            (extension "rs" "c" "cpp" "h" "hpp" "cc" "rb" "py" "el" "html" "js" "css" "jl" "rs" "m" "v" "hs" "lhs" "pl"))
           ("Text"
            (extension "md" "rst" "txt"))
           ("Org"
            (extension "org"))
           ("Archives"
            (extension "zip" "rar" "tar" "gz" "bz2" "xz"))
           ("Images"
            (extension "jpg" "jpeg" "webp" "png" "bmp" "gif" "tiff" "xcf"))))))

(use-package all-the-icons-dired
  :after (dired)
  :init (all-the-icons-dired-mode 1))

(provide 'prelude-dired)
