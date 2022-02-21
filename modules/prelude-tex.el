;;; -*- lexical-binding: t; -*-

(use-package tex
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config
  (setq-default TeX-engine 'xetex)
  (add-hook 'LaTeX-mode-hook
            #'(lambda ()
                (setq fill-column 80)
                (setq TeX-master t)))

  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  (setq TeX-auto-save t
        TeX-parse-self t)
  
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (when k|mac
    (setq TeX-source-correlate-mode 'synctex
          TeX-view-program-list
          '(("Skim"  "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b"))
          TeX-view-program-selection '((output-pdf "Skim")))))

(use-package reftex
  :ensure nil
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :hook ((latex-mode LaTeX-mode) . turn-on-cdlatex)
  :config
  (setq cdlatex-command-alist
      '(("sum" "Insert \\sum_{}^{}"
         "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
        ("prd" "Insert \\prod_{}^{}"
         "\\prod_{?}^{}" cdlatex-position-cursor nil nil t))))

(provide 'prelude-tex)
