(ensure-package 'auctex)

;; Prefer XeTeX; it should be compatible with the default engine
(setq-default TeX-engine 'xetex)

;; Use CDLaTeX for faster typing
(ensure-package 'cdlatex)

(add-hook 'latex-mode-hook #'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)

(provide 'prelude-tex)
