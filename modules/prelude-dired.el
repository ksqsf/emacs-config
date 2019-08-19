;;; -*- lexical-binding: t; -*-
;; Search file names when point is at a file name; Search unlimitedly
;; otherwise.
(setq dired-isearch-filenames 'dwim)

;; Intelligently guess the target directory.
(setq dired-dwim-target t)

;; Filters
(ensure-package 'dired-filter)
(add-hook 'dired-mode-hook #'dired-filter-group-mode)
(add-hook 'dired-mode-hook #'dired-filter-mode)
(define-key dired-mode-map (kbd "/") dired-filter-map)
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
          (extension "c" "cpp" "h" "hpp" "cc" "rb" "py" "el" "html" "js" "css"))
         ("Text"
          (extension "md" "rst" "txt"))
         ("Org"
          (extension "org"))
         ("Archives"
          (extension "zip" "rar" "tar" "gz" "bz2" "xz"))
         ("Images"
          (extension "jpg" "jpeg" "webp" "png" "bmp" "gif" "tiff" "xcf")))))

;; Colors
(ensure-package 'dired-rainbow)
(eval-after-load "dired"
  (progn
    (require 'dired-rainbow)
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html        "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml         "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document    "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown    "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database    "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media       "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image       "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log         "#c17d11" ("log"))
    (dired-rainbow-define shell       "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled    "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable  "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed  "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged    "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted   "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts       "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition   "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc          "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))


(provide 'prelude-dired)
