;;; -*- lexical-binding: t; -*-

(use-package tex
  :defer t
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config
  (setq-default TeX-engine 'xetex))

(use-package cdlatex
  :hook ((latex-mode LaTeX-mode) . turn-on-cdlatex)
  :config
  (setq cdlatex-command-alist
      '(("sum" "Insert \\sum_{}^{}"
         "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
        ("prd" "Insert \\prod_{}^{}"
         "\\prod_{?}^{}" cdlatex-position-cursor nil nil t))))

(provide 'prelude-tex)
