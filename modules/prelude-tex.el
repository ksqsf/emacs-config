;;; -*- lexical-binding: t; -*-

(use-package tex
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :commands (TeX-revert-document-buffer TeX-PDF-mode TeX-source-correlate-mode)
  :config
  (setq-default TeX-engine 'xetex)
  (add-hook 'LaTeX-mode-hook
            #'(lambda ()
                (setq fill-column 80)))

  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)

  (setq-default LaTeX-electric-left-right-brace t)

  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook #'+latex-setup)

  (defun +latex-setup ()
    ;; remove { and } so that cape-file can work inside {}
    (setq-local thing-at-point-file-name-chars "-@~/[:alnum:]_.$#%,:"))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(latex-mode "texlab"))))

(use-package reftex
  :ensure nil
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :defer t
  ;; :hook ((latex-mode LaTeX-mode) . turn-on-cdlatex)
  :config
  (setq cdlatex-command-alist
        '(("sum" "Insert \\sum_{}^{}" "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
          ("prd" "Insert \\prod_{}^{}" "\\prod_{?}^{}" cdlatex-position-cursor nil nil t)
          ("tt" "Insert \\texttt{}" "\\texttt{?}" cdlatex-position-cursor nil t t)
          ("fr" "Insert \\frac{}{}" "\\frac{?}{?}" cdlatex-position-cursor nil nil t)))
  (setq cdlatex-env-alist
        '(("frame" "\\begin{frame}\n\\frametitle{?}\n\n\\end{frame}" nil)))
  (setq cdlatex-math-symbol-alist
        '((?0 ("\\varnothing" "\\emptyset"))
          (?1 ("\\ONE" "\\one"))
          (?. ("\\cdot" "\\circ"))
          (?v ("\\vee" "\\bigvee"))
          (?& ("\\wedge" "\\bigwedge"))
          (?~ ("\\sim" "\\approx" "\\propto"))))
  (setq cdlatex-parens-pairs "$[{("))

(use-package typst-ts-mode
  :vc (:fetcher sourcehut :repo "meow_king/typst-ts-mode")
  :mode ("\\.typ\\'" . typst-ts-mode))

(provide 'prelude-tex)
