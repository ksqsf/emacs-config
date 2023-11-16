;;; -*- lexical-binding: t; -*-

(use-package tex
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :commands (TeX-revert-document-buffer TeX-PDF-mode TeX-source-correlate-mode)
  :config
  (setq-default TeX-engine 'luatex)
  (add-hook 'LaTeX-mode-hook
            #'(lambda ()
                (setq fill-column 80)))

  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)

  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

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

(provide 'prelude-tex)
