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
;; Currently, it supports syntax highlighting, indentation, and imenu
;; integration.

;;; Code:

(require 'rx)
(require 'imenu)
(require 'flymake)

(eval-when-compile
  (require 'cl))

(defvar pest--highlights
  `((,(rx (or "SOI" "EOI" "@" "+" "*" "?" "~"))         . font-lock-keyword-face)
    (,(rx "//" (* nonl) eol)                            . font-lock-comment-face)
    (,(rx "'" (char alpha) "'")                         . font-lock-string-face)
    (,(rx (+ (or alpha "_")) (* (or (char alnum) "_"))) . font-lock-variable-name-face)))

(defun pest-indent-line (&optional indent)
  "Indent the current line according to the Pest syntax."
  (interactive "P")
  (let ((pos (- (point-max) (point)))
        (indent (or indent (calculate-pest-indentation)))
        (shift-amt nil)
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (null indent)
        (goto-char (- (point-max) pos))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent))
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

(defun calculate-pest-indentation ()
  "Calculate the indentation of the current line."
  (let (indent)
    (save-excursion
      (back-to-indentation)
      (let* ((ppss (syntax-ppss))
             (depth (car ppss))
             (paren-start-pos (cadr ppss))
             (base (* 4 depth))
             (rule-sep (save-excursion
                         (or (looking-at "|")
                             (re-search-backward "|" paren-start-pos t)))))
        (unless (= depth 0)
          (setq indent base)
          (if (looking-at "\\s)")
              (setq indent (- base 4))
            (if (null rule-sep)
              (setq indent (+ 2 base)))))))
    indent))



(defun pest-imenu-prev-index-position ()
  (interactive)
  (re-search-backward (rx bol
                          (group (+ (or alpha "_") (* (or (char alnum) "_"))))
                          (* blank)
                          "=" (* blank) (or "_{" "@{" "!{" "${" "{"))
                      (point-min) t))

(defun pest-imenu-extract-index-name ()
  (interactive)
  (match-string-no-properties 1))



(defvar-local pest--flymake-proc nil)

(defun pest-flymake (report-fn &rest _args)
  (unless (executable-find "pesta")
    (error "Cannot find a suitable `pesta' executable"))
  (when (process-live-p pest--flymake-proc)
    (kill-process pest--flymake-proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq pest--flymake-proc
            (make-process
             :name "pest-flymake"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *pest-flymake*")
             :command '("pesta")
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (if (with-current-buffer source (eq proc pest--flymake-proc))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           (cl-loop
                            while (search-forward-regexp (rx bol
                                                             (group (or "nil")) " "
                                                             (group (+ (char digit))) " "
                                                             "(" (+ (char digit)) "," (+ (char digit)) ") "
                                                             (group (* nonl))
                                                             eol)
                                                         nil t)
                            for msg = (match-string 3)
                            for beg = (string-to-number (match-string 2))
                            for type = :error
                            collect (flymake-make-diagnostic source
                                                             beg
                                                             (1+ beg)
                                                             type
                                                             msg)
                            into diags
                            finally (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s"
                                    proc))
                   (kill-buffer (process-buffer proc)))))))
      (process-send-region pest--flymake-proc (point-min) (point-max))
      (process-send-eof pest--flymake-proc))))

;;;###autoload
(define-derived-mode pest-mode prog-mode "Pest"
  "Major mode for editing Pest files"
  (setq-local font-lock-defaults '(pest--highlights))
  (setq-local indent-line-function #'pest-indent-line)
  (setq-local imenu-prev-index-position-function #'pest-imenu-prev-index-position)
  (setq-local imenu-extract-index-name-function #'pest-imenu-extract-index-name)
  (add-hook 'flymake-diagnostic-functions 'pest-flymake nil t))

(provide 'pest-mode)
