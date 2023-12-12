#!/usr/bin/env emacs -x

(require 'package)
(package-initialize)

;; add some large packages here
(require 'org)
(require 'magit)
(require 'org-protocol)
(require 'telega)

(setq +saved-load-path-during-dump load-path)
(dump-emacs-portable "core.pdmp")
