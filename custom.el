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
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "fc9ff61d51078365f4b1eb5a8f47b71700f967743b181c0ccae1401956e4b568" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "e8825f26af32403c5ad8bc983f8610a4a4786eb55e3a363fa9acb48e0677fe7e" "8cb818e0658f6cc59928a8f2b2917adc36d882267bf816994e00c5b8fcbf6933" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "233bb646e100bda00c0af26afe7ab563ef118b9d685f1ac3ca5387856674285d" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "b563a87aa29096e0b2e38889f7a5e3babde9982262181b65de9ce8b78e9324d5" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "b181ea0cc32303da7f9227361bb051bbb6c3105bb4f386ca22a06db319b08882" "d21135150e22e58f8c656ec04530872831baebf5a1c3688030d119c114233c24" "c616e584f7268aa3b63d08045a912b50863a34e7ea83e35fcab8537b75741956" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" default)))
 '(default-input-method "chinese-wbim")
 '(ede-project-directories
   (quote
    ("/tmp/myproject/include" "/tmp/myproject/src" "/tmp/myproject")))
 '(fci-rule-color "#eee8d5")
 '(global-centered-cursor-mode t)
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
 '(ibuffer-saved-filter-groups
   (quote
    (("Work"
      ("Work"
       (filename . "work"))
      ("IRC"
       (used-mode . erc-mode))))))
 '(ibuffer-saved-filters
   (quote
    (("gnus"
      ((or
	(mode . message-mode)
	(mode . mail-mode)
	(mode . gnus-group-mode)
	(mode . gnus-summary-mode)
	(mode . gnus-article-mode))))
     ("programming"
      ((or
	(mode . emacs-lisp-mode)
	(mode . cperl-mode)
	(mode . c-mode)
	(mode . java-mode)
	(mode . idl-mode)
	(mode . lisp-mode)))))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-babel-load-languages (quote ((dot . t) (emacs-lisp . t))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (ycmd php-mode elisp-docstring-mode string-edit bison-mode nyan-mode typescript-mode chinese-yasdcv all-the-icons chinese-wbim neotree org-download intero ox-reveal centered-cursor-mode yasnippet-snippets company-lsp minimap projectile-ripgrep ido-completing-read+ flx-ido ido-vertical-mode helm zenburn-theme lsp-rust z3-mode boogie-friends nova-theme org-drill-table spacemacs-theme farmhouse-theme plan9-theme solarized-theme moe-theme monokai-theme monokai-alt-theme molokai-theme edit-indirect lenlen-theme dracula-theme powerline org-plus-contrib ox-twbs org-bullets docker disaster auctex graphviz-dot-mode yaml-mode restclient toml-mode ripgrep xkcd mips-mode use-package magit yasnippet rust-mode racer elpy multiple-cursors ripgrep company flycheck markdown-mode pdf-tools company-c-headers cargo flycheck-rust cnfonts paredit)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   (quote
    ((pony-settings
      (make-pony-project :python "./venv/bin/python3" :pythonpath "./venv")))))
 '(send-mail-function (quote sendmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
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
 '(yasdcv-sdcv-dicts
   (quote
    (("jianminghy" "简明汉英词典" "powerword2007" nil)
     ("jianmingyh" "简明英汉词典" "powerword2007" nil)
     ("lanconghy" "懒虫简明汉英词典" nil nil)
     ("lancongyh" "懒虫简明英汉词典" nil nil)
     ("xdictyh" "XDICT英汉辞典" nil t)
     ("xdicthy" "XDICT汉英辞典" nil t)
     ("xiandai" "现代英汉综合大辞典" "powerword2007" t)
     ("niujing" "牛津高阶英汉双解" "oald" nil)
     ("niujin" "牛津现代英汉双解词典" nil t)
     ("" "英文相关词典" "powerword2007" nil)
     ("langdaohy" "朗道汉英字典5.0" "langdao" nil)
     ("langdaoyh" "朗道英汉字典5.0" "langdao" nil)
     ("21shiji" "21世纪英汉汉英双向词典" "21cen" nil)
     ("21shjikj" "21世纪双语科技词典" " nil" nil)
     ("" "新世纪英汉科技大词典" nil nil)
     ("" "新世纪汉英科技大词典" nil nil)
     ("" "现代商务汉英大词典" "powerword2007" nil)
     ("" "英汉双解计算机词典" "powerword2007" nil)
     ("" "汉语成语词典" "chengyu" t)
     ("" "高级汉语大词典" nil nil)
     ("" "现代汉语词典" nil nil)
     ("" "Cantonese Simp-English" nil nil)
     ("" "英汉进出口商品词汇大全" nil nil)
     ("" "中国大百科全书2.0版" nil t)
     ("" "CEDICT汉英辞典" nil nil)
     ("" "英文字根字典" nil t)
     ("" "湘雅医学专业词典" nil nil)
     ("" "[七国语言]英汉化学大词典" "powerword2007" nil)
     ("" "[七国语言]英汉数学大词典" "powerword2007" nil)
     ("" "[七国语言]英汉公共大词典" "powerword2007" nil)
     ("" "[七国语言]英汉医学大词典" "powerword2007" nil)
     ("" "[七国语言]英汉信息大词典" "powerword2007" nil)
     ("" "[七国语言]英汉生物学大词典" "powerword2007" nil)
     ("" "[名词委审定]英汉铁道科技名词" "powerword2007" nil)
     ("" "[名词委审定]汉英细胞生物学名词" "powerword2007" nil)
     ("" "[名词委审定]汉英数学名词" "powerword2007" nil)
     ("" "[名词委审定]汉英医学名词(七, 整形、美容、皮肤、康复)" "powerword2007" nil)
     ("" "[名词委审定]汉英医学名词(四, 心血管病学等)" "powerword2007" nil)
     ("" "[名词委审定]汉英医学名词(一, 妇产科学)" "powerword2007" nil)
     ("" "[名词委审定]汉英生物化学名词" "powerword2007" nil)
     ("" "[名词委审定]英汉生物化学名词" "powerword2007" nil)
     ("" "[名词委审定]汉英医学名词(二, 口腔学)" "powerword2007" nil)
     ("" "[名词委审定]汉英医学名词(六, 外科)" "powerword2007" nil)
     ("" "[名词委审定]汉英人体解剖学名词" "powerword2007" nil)
     ("" "[名词委审定]汉英药学名词" "powerword2007" nil)
     ("" "[名词委审定]汉英医学名词(三, 遗传学等)" "powerword2007" nil)
     ("" "[名词委审定]汉英医学名词(五, 眼科学)" "powerword2007" nil)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
