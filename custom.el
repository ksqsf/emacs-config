(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(ccm-vpos-init (quote (round (* 21 (ccm-visible-text-lines)) 34)))
 '(company-backends
   (quote
    (company-irony-c-headers company-irony company-c-headers company-nxml company-css company-cmake company-capf company-files
			     (company-dabbrev-code company-gtags company-etags company-keywords)
			     company-dabbrev)))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(eww-search-prefix "https://www.google.com/search?q=")
 '(fancy-splash-image nil)
 '(fci-rule-color "#eee8d5")
 '(gdb-mi-decode-strings t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(icomplete-compute-delay 0.1)
 '(icomplete-show-matches-on-no-input t)
 '(ispell-dictionary "english")
 '(line-spacing 0.2)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(my-blog-posts-dir "/home/ksqsf/Site/jekyll/_posts/" t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-babel-inline-result-wrap "=%s=")
 '(org-babel-load-languages (quote ((dot . t) (emacs-lisp . t) (C . t))))
 '(org-babel-tangle-lang-exts
   (quote
    (("emacs-lisp" . "el")
     ("elisp" . "el")
     ("c++" . "cpp")
     ("python" . "py")
     ("rust" . "rs"))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(org-todo-keywords (quote ((sequence "TODO" "WAITING" "DONE"))))
 '(package-selected-packages
   (quote
    (pinyinlib counsel-projectile counsel tide rainbow-delimiters yasdcv smex dired-x helpful which-key hydra mips-mode racer monokai-theme windresize winum company-irony-c-headers irony-eldoc company-irony flycheck-irony irony ggtags eglot php-mode elisp-docstring-mode string-edit nyan-mode chinese-yasdcv all-the-icons chinese-wbim neotree org-download ox-reveal company-lsp projectile-ripgrep flx-ido org-drill-table edit-indirect powerline org-plus-contrib ox-twbs org-bullets disaster auctex graphviz-dot-mode yaml-mode restclient toml-mode magit elpy multiple-cursors flycheck markdown-mode company-c-headers cargo flycheck-rust paredit)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments lambda nil 1)
     (pony-settings
      (make-pony-project :python "./venv/bin/python3" :pythonpath "./venv")))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(undo-limit 800000)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(when
      (or
       (not
	(boundp
	 (quote ansi-term-color-vector)))
       (not
	(facep
	 (aref ansi-term-color-vector 0)))))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yasdcv-sdcv-dicts (quote (("niujin" "牛津现代英汉双解词典" nil t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(icomplete-first-match ((t (:foreground "#ff79c6" :weight bold))))
 '(markdown-table-face ((t (:inherit org-table))))
 '(org-table ((t (:foreground "LightSkyBlue" :family "Sarasa Term TC")))))
