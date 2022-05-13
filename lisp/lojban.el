;;; lojban.el --- The language environment for Lojban, a logical language  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ksqsf

;; Author: ksqsf <i@ksqsf.moe>
;; Keywords: i18n, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package currently contains required setup and a Quail input
;; method for Zbalermorna, a writing system for Lojban.

;; To display zbalermorna text correctly, you will need
;;
;; 1. Download and install a font for zbalermorna from
;;    https://github.com/jackhumbert/zbalermorna/tree/master/fonts/.
;;
;;    (OPTIONAL) If you have other fonts that conflict with
;;    zbalermorna codepoints, consider using setting
;;    `zbalermorna-font'.
;;
;; 2. Call `zbalermorna-setup', or use <C-x RET l> and choose Lojban,
;;    at least once.

;; To globally enable zbalermorna, consider adding config to your init
;; file:
;;
;;     (require 'lojban)
;;     (setq zbalermorna-font "Crisa")
;;     (zbalermorna-setup)

;;; Code:

(require 'quail)

(defun zbalermorna-font-setup (font-family)
  "Specify FONT-FAMILY to be used for the Unicode range of zbalermorna.

The fonts can be downloaded from https://github.com/jackhumbert/zbalermorna/tree/master/fonts/.

Example: (zbalermorna-font-setup \"Crisa\")"
  (set-fontset-font t '(#xed80 . #xedbf) font-family))

(defgroup lojban nil
  "The customization group for Lojban, a logical language.")

(defun zbalermorna--set-font (symbol value)
  (set-default symbol value)
  (when (not (string-empty-p value))
    (zbalermorna-font-setup value)))

(defcustom zbalermorna-font ""
  "The default font used for zbalermona. If empty, the font will be
chosen by Emacs automatically.

The change immediately takes effect after you modify it using the
Custom interface."
  :type 'string
  :group 'lojban
  :set 'zbalermorna--set-font)

(quail-define-package
 "zbalermorna" "Lojban" "" t
 "Compose-like input method for zbalermorna.

Examples:
 zbalermorna -> "
 nil t t nil nil nil nil nil nil nil t)

(defconst zbalermorna-cnimaho ?
  "The attitudinal shorthand for ._'_

Usually such a shorthand shape is created automatically from
ligatures. The input method will not generate this character.")

(quail-define-rules
 ;; consonants
 ("p" 60800) ("t" 60801) ("k" 60802) ("f" 60803)
 ("l" 60804) ("s" 60805) ("c" 60806) ("m" 60807)
 ("x" 60808) ("." 60809) ("h" 60810) ("'" 60810)
 ("b" 60816) ("d" 60817) ("g" 60818) ("v" 60819)
 ("r" 60820) ("z" 60821) ("j" 60822) ("n" 60823)

 ;; vowels
 ("a" 60832) ("e" 60833) ("i" 60834) ("o" 60835)
 ("u" 60836) ("y" 60837) ("ai" 60838) ("ei" 60839)
 ("oi" 60840) ("au" 60841)

 ;; full vowels
 ("A" 60848) ("E" 60849) ("I" 60850) ("O" 60851)
 ("U" 60852) ("Y" 60853) ("AI" 60854) ("EI" 60855)
 ("OI" 60856) ("AU" 60857)

 ("`" 60824)  ;; strech
 ("-" 60825)  ;; pause
 ("~" 60827)  ;; strech

 ;; intonation
 ("1" 60812) ("2" 60813) ("3" 60814) ("4" 60815)

 ;; semivowels
 ("q" 60842)
 ("w" 60843))

;;;###autoload
(defun zbalermorna-setup ()
  "Set up the composition rules for zbalermonrna."
  (interactive)

  (when (not (string= zbalermorna-font ""))
    (zbalermorna-font-setup zbalermorna-font))

  (dolist (v (number-sequence #xeda0 #xeda9))
    (put-char-code-property v 'canonical-combining-class (encode-composition-rule '(tc . bc))))

  (let* ((c "\\([\uED80-\uED97]\\|\uEDAA\\|\uEDAB\\)")
         (v "[\uEDA0-\uEDA9]")
         (dot "\uED89")
         (h "\uED8A")
         (pattern1 (concat c v))
         (pattern2 (concat v h v)))
    (set-char-table-range
     composition-function-table '(#xeda0 . #xeda9)
     (list (vector pattern2 2 #'compose-gstring-for-graphic)
           (vector pattern1 1 #'compose-gstring-for-graphic)
           [nil 0 font-shape-gstring]))))

;;;###autoload
(defun lojban-setup ()
  "Set up the Lojban language environment."
  (interactive)
  (zbalermorna-setup))

;;;###autoload
(set-language-info-alist
 "Lojban"
 '((charset unicode)
   (coding-system utf-8)
   (coding-priority utf-8)
   (input-method . "zbalermorna")
   (documentation . "\
Using the zbalermorna script and its associated input method.")
   (sample-text . "         ")
   (setup-function . lojban-setup)))

(provide 'lojban)
;;; lojban.el ends here
