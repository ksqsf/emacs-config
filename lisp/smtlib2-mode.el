;;; smtlib2-mode.el --- A SMTLIBv2 interactive development environment -*-lexical-binding: t-*-

;; Version: 0.0.1
;; Author: ksqsf <i@ksqsf.moe>
;; Homepage: https://github.com/ksqsf/emacs-config
;; Keywords: z3 yices mathsat cvc4 cvc5 smt beaver
;; Package-Requires: ((emacs "27"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An interactive development environment for SMT-LIB files. All SMT
;; solvers that support the SMT-LIBv2 format are supported
;; automatically.

;; This package is a fork of Zephyr Pellerin's z3-mode. I enhanced the
;; package to support:

;; - Proper indentation for several constructs, e.g. forall, declare-*.
;; - Highlighting for more commands
;; - Limited support for SyGuS

;;; Code:
(defgroup smtlib2 nil
  "SMTLib2 Mode"
  :group 'languages
  :prefix "smtlib2-")

(defcustom smtlib2-solver-cmd "z3"
  "The command used when you run the solver."
  :type 'file
  :group 'smtlib2)



(defvar smtlib2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'smtlib2-execute-region)
    map)
  "Keymap for smtlib2-mode.")


;; font-lock support

;; Matches alternative base numeric primitives such as `#xF0FA' & `#b010'
(defconst smtlib2-altbase-literal-regexp "\\(#x[0-9a-fA-F]+\\|#b[01]+\\)")

; Matches the simplest symbol regexp format
(defconst smtlib2-symbol-regexp "[a-zA-Z~!@$%^&*_+=<>.?/-][0-9a-zA-Z~!@$%^&*_+=<>.?/-]*")

;; Matches an alternative quote symbol regexp format
(defconst smtlib2-quoted-symbol-regexp "|[]!-[^-{}~ \t\r\n]*|")

;; Matches lisp-symbol style keywords, i.e `:keyword'
(defconst smtlib2-keyword-symbol-regexp ":[0-9a-zA-Z~!@$%^&*_+=<>.?/-]+")

;; Reserved words as defined in the SMT-LIB standard
(defconst smtlib2-keywords
  '("!" "_" "as" "exists" "forall" "let" "match" "par"
    "assert" "check-sat" "check-sat-assuming" "declare-const"
    "declare-datatype" "declare-datatypes" "declare-fun" "declare-sort"
    "define-fun" "define-fun-rec" "define-sort" "echo" "exit" "get-assertions"
    "get-assignment" "get-info" "get-model" "get-option" "get-proof"
    "get-unsat-assumptions" "get-unsat-core" "get-value" "pop" "push"
    "reset" "reset-assertions" "set-info" "set-logic" "set-option"

    ;; SyGuS
    "synth-fun" "constraint" "check-synth"
    )
  "Commands and other keywords defined by the SMT-LIB standard.")

;; Define our font-lock
(defvar smtlib2-keywords-regexp (regexp-opt smtlib2-keywords 'words))

(defvar smtlib2-font-lock-defaults
  `(((,smtlib2-keywords-regexp . font-lock-keyword-face)
     (,smtlib2-keyword-symbol-regexp . font-lock-builtin-face)
     (,smtlib2-altbase-literal-regexp . font-lock-constant-face)
     ;; We *can* highlight symbols, but it compromises the clarity
     ;; (,smtlib2-symbol-regexp . font-lock-function-name-face)
     )))


(let ((l '((forall 1)
           (exists 1)
           (let 1)
           (match 1)
           (par 1)
           (declare-const 2)
           (declare-datatype 1)
           (declare-datatypes 1)
           (declare-fun 3)
           (declare-sort 1)
           (define-fun 3)
           (define-fun-rec 3)
           (define-sort 1)
           (ite 2)
           (synth-fun 3))))
  (dolist (el l)
    (put (car el) 'common-lisp-indent-function
         (if (symbolp (cdr el))
             (get (cdr el) 'common-lisp-indent-function)
           (car (cdr el))))))


;; mode-command and utility functions

;; Define the mode
;;;###autoload
(define-derived-mode smtlib2-mode lisp-mode "SMT2"
  "Major mode for editing SMT-LIB2 files"
  (setq font-lock-defaults smtlib2-font-lock-defaults)
  ;; use cl-indent
  (require 'cl-indent)
  (setq-local lisp-indent-function 'common-lisp-indent-function)
  ;; disable sly-mode
  (when (fboundp 'sly-editing-mode)
    (sly-editing-mode -1))
  (when (fboundp 'sly-mode)
    (sly-mode -1)))

;; Command to run SMT solver on the whole buffer
(defun smtlib2-execute-region ()
  "Pass optional header and region to a prover for noninteractive execution.
The working directory is that of the buffer, and only environment variables
are already set which is why you can mark a header within the script."
  (interactive)
  (shell-command-on-region (if (region-active-p) (region-beginning) (point-min))
                           (if (region-active-p) (region-end) (point-max))
                           smtlib2-solver-cmd))



(setq auto-mode-alist
      (append
       '(("\\.smt[2]?\\'" . smtlib2-mode)
         ("\\.sygus[2]?\\'" . smtlib2-mode)) auto-mode-alist))

(provide 'smtlib2-mode)

;;; smtlib2-mode.el ends here
