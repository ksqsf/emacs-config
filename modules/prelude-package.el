;;; -*- lexical-binding: t; -*-
;;; Packages.
;;;
;;; package.el:
;;; * No version pinning

;; Mirrors moved to early-init.el

;; use-package
(require 'use-package)

;; Set up use-package for user config
(setq use-package-always-ensure t)  ; All packages used have to be installed

;; Marry package-vc with use-package.  Quelpa is too heavy.
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(provide 'prelude-package)
