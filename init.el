;;; -*- lexical-binding: t; -*-
;; I don't support any version other than what I use.
(when (version< emacs-version "28.1")
  (warn "This configuration is only tested on Emacs 28.1+"))

;; Directories.
(defvar prelude-lisp-dir (expand-file-name "lisp" user-emacs-directory)
  "This directory contains third-party Lisp files.")
(defvar prelude-modules-dir (expand-file-name "modules" user-emacs-directory)
  "The directory contains all modules.")
(defvar prelude-volatile-dir (expand-file-name "volatile" user-emacs-directory)
  "This directory contains volatile configuration.  All Lisp
  files are loaded automatically.  You shouldn't byte-compile
  these Lisp files.  To disable a file, just change the extension
  from .el to whatever else.")

(add-to-list 'load-path prelude-lisp-dir)
(add-to-list 'load-path prelude-modules-dir)

;;; Set up autoloads from prelude-lisp-dir.
(require 'prelude-loaddefs)

;; Customization.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Modules.
(require 'prelude-package)
(require 'prelude-common)

(require 'prelude-benchmark)
(require 'prelude-core)
(require 'prelude-ui)
(require 'prelude-project)
(require 'prelude-completion)

;; (require 'prelude-meow)
;; (require 'prelude-evil)
(require 'prelude-nix)

(require 'prelude-search)
(require 'prelude-ibuffer)
(require 'prelude-chinese)
(require 'prelude-dired)
(require 'prelude-git)
(require 'prelude-tex)
(require 'prelude-org)
(require 'prelude-blog)

(require 'prelude-prog)
(require 'prelude-lang-lisp)
(require 'prelude-lang-cc)
(require 'prelude-lang-python)
(require 'prelude-lang-rust)
(require 'prelude-lang-coq)
(require 'prelude-lang-ml)
(require 'prelude-lang-js)
(require 'prelude-lang-haskell)
(require 'prelude-lang-agda)
(require 'prelude-lang-web)
(require 'prelude-lang-zig)

(require 'prelude-help)
(require 'prelude-os)
(require 'prelude-mail)
(require 'prelude-apps)
(require 'prelude-irc)

;; Volatile.
(when (file-exists-p prelude-volatile-dir)
  (message "Loading volatile configuration files in %s..." prelude-volatile-dir)
  (mapc 'load (directory-files prelude-volatile-dir 't "^[^#\.].*el$")))

(put 'dired-find-alternate-file 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'window-swap-states 'disabled t)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
