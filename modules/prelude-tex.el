(ensure-package 'auctex)

;; Prefer XeTeX; it should be compatible with the default engine
(setq-default TeX-engine 'xetex)

;; Use CDLaTeX for faster typing
(ensure-package 'cdlatex)

(add-hook 'latex-mode-hook #'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)

(setq cdlatex-command-alist
      '(("sum" "Insert \\sum_{}^{}"
         "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
        ("prd" "Insert \\prod_{}^{}"
         "\\prod_{?}^{}" cdlatex-position-cursor nil nil t)))

(provide 'prelude-tex)
