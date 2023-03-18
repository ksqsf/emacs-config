;;; -*- lexical-binding: t; -*-
;;; Packages.
;;;
;;; package.el:
;;; * No version pinning

;; Mirrors moved to early-init.el

;; use-package
(require 'use-package)

;; NOTE: quelpa requires package, and hence package-quickstart will be
;; useless.  (According to someone familiar with this matter.)
(use-package quelpa
  :ensure t
  :demand t
  :init
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-upgrade-p nil)
  (setq quelpa-dir (no-littering-expand-var-file-name "quelpa/")
        quelpa-build-dir (no-littering-expand-var-file-name "quelpa/build/")
        quelpa-melpa-dir (no-littering-expand-var-file-name "quelpa/melpa/")
        quelpa-packages-dir (no-littering-expand-var-file-name "quelpa/packages/")
        quelpa-persistent-cache-file (no-littering-expand-var-file-name "quelpa/cache")))

(use-package quelpa-use-package
  :ensure t
  :demand t
  :config
  (quelpa-use-package-activate-advice))

;; Set up use-package for user config
(setq use-package-always-ensure t)  ; All packages used have to be installed

(provide 'prelude-package)
