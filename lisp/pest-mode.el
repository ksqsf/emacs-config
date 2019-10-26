;;; pest-mode.el --- Major mode for editing Pest files -*- lexical-binding: t; -*-

;; Author: ksqsf <i@ksqsf.moe>
;; URL: https://github.com/ksqsf/pest-mode.el
;; Version: 0.1
;; Package-Requires: ((emacs "26.3"))

;; Copyright (C) 2019 ksqsf

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides GNU Emacs major modes for editing Pest grammar files.
;; Currently, it supports syntax highlighting.

;;; Code:

(require 'rx)

(setq pest--highlights
      `((,(rx (or "SOI" "EOI" "@" "+" "*" "~")) . font-lock-keyword-face)
        (,(rx "//" (* nonl) eol)            . font-lock-comment-face)
        (,(rx "'" (char alpha) "'")         . font-lock-string-face)
        (,(rx (+ (or alpha "_")) (* (or (char alnum) "_"))) . font-lock-variable-name-face)
        ))

(define-derived-mode pest-mode prog-mode "Pest"
  "Major mode for editing Pest files"
  (setq font-lock-defaults '(pest--highlights)))

(provide 'pest-mode)
