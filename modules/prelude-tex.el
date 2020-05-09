;;; -*- lexical-binding: t; -*-

(use-package tex
  :defer t
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config
  (setq-default TeX-engine 'xetex)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq fill-column 80)
              (setq TeX-master (expand-file-name
                                "main.tex"
                                (projectile-project-root))))))

(use-package cdlatex
  :hook ((latex-mode LaTeX-mode) . turn-on-cdlatex)
  :config
  (setq cdlatex-command-alist
      '(("sum" "Insert \\sum_{}^{}"
         "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
        ("prd" "Insert \\prod_{}^{}"
         "\\prod_{?}^{}" cdlatex-position-cursor nil nil t))))

(provide 'prelude-tex)
