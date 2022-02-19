;;; -*- lexical-binding: t; -*-
;;; Packages.
;;;
;;; package.el:
;;; * No version pinning

;; Mirrors moved to early-init.el

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))
(setq use-package-always-ensure t)      ; All packages used have to be installed

;; quelpa
(use-package quelpa
  :after (no-littering)
  :init
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-dir (no-littering-expand-var-file-name "quelpa/")
        quelpa-build-dir (no-littering-expand-var-file-name "quelpa/build/")
        quelpa-melpa-dir (no-littering-expand-var-file-name "quelpa/melpa/")
        quelpa-packages-dir (no-littering-expand-var-file-name "quelpa/packages/")
        quelpa-persistent-cache-file (no-littering-expand-var-file-name "quelpa/cache")))
(use-package quelpa-use-package)
(quelpa-use-package-activate-advice)    ; Don't consult melpa when :quelpa is found

(provide 'prelude-package)
