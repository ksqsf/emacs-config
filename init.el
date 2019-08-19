;;; -*- lexical-binding: t; -*-
;; I don't support any version other than what I use.
(when (version< emacs-version "26.1")
  (warn "This configuration is only tested on Emacs 26.1"))

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

;; Modules.
(require 'prelude)
;(require 'prelude-benchmark)
(require 'prelude-core)
(require 'prelude-ui)
(require 'prelude-ivy)
(require 'prelude-project)
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

(require 'prelude-utils)
(require 'prelude-other)

;; Volatile.
(when (file-exists-p prelude-volatile-dir)
  (message "Loading volatile configuration files in %s..." prelude-volatile-dir)
  (mapc 'load (directory-files prelude-volatile-dir 't "^[^#\.].*el$")))
