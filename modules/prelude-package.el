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
(require 'vc-use-package)

;; Mask package-quickstart before doing batch operations
(advice-add 'package-menu-execute :around
            (lambda (oldfun &rest args)
              (let ((package-quickstart nil))
                (apply oldfun args))
              (when package-quickstart
                (package-quickstart-refresh))))

(provide 'prelude-package)
