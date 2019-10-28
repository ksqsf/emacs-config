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
(require 'json)

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



(defvar pest--rule-regexp (rx bol
                              (group (+ (or alpha "_") (* (or (char alnum) "_"))))
                              (* blank)
                              "=" (* blank) (or "_{" "@{" "!{" "${" "{")))

(defun pest-imenu-prev-index-position ()
  (interactive)
  (re-search-backward pest--rule-regexp (point-min) t))

(defun pest-imenu-extract-index-name ()
  (interactive)
  (match-string-no-properties 1))

(defun pest--rule-list (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (cl-loop
           while (re-search-forward pest--rule-regexp nil t)
           collect (match-string-no-properties 1)))))))



(defvar pest--diagnosis-regexp (rx bol
                                   "nil "
                                   "(" (group (+ (char digit))) "," (group (+ (char digit))) ") "
                                   (group (* nonl))
                                   eol))

(defvar-local pest--meta-flymake-proc nil)

(defun pest-flymake (report-fn &rest _args)
  (unless (executable-find "pesta")
    (error "Cannot find a suitable `pesta' executable"))
  (when (process-live-p pest--meta-flymake-proc)
    (kill-process pest--meta-flymake-proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq pest--meta-flymake-proc
            (make-process
             :name "pest-meta-flymake"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *pest-meta-flymake*")
             :command '("pesta" "meta_check")
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (if (with-current-buffer source (eq proc pest--meta-flymake-proc))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           (cl-loop
                            while (search-forward-regexp pest--diagnosis-regexp
                                                         nil t)
                            for msg = (match-string 3)
                            for beg = (string-to-number (match-string 1))
                            for end = (string-to-number (match-string 2))
                            for type = :error
                            collect (flymake-make-diagnostic source
                                                             beg
                                                             end
                                                             type
                                                             msg)
                            into diags
                            finally (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s"
                                    proc))
                   (kill-buffer (process-buffer proc)))))))
      (process-send-region pest--meta-flymake-proc (point-min) (point-max))
      (process-send-eof pest--meta-flymake-proc))))



(defun pest-test-grammar ()
  "Test the grammar in the current buffer on arbitrary input in a
newly-created buffer, with real-time diagnosis messages."
  (interactive)
  (let ((grammar-buffer (current-buffer))
        (input-buffer (switch-to-buffer-other-window "*pest-input*")))
    (message "Associate with grammar %s" grammar-buffer)
    (pest-input-mode)
    (setq-local pest--grammar-buffer grammar-buffer)))



(defvar pest-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Pest")))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "C-c C-t") 'pest-test-grammar)
    (define-key map [menu-bar pest] (cons "Pest" menu-map))
    (define-key menu-map [test-grammar]
      '(menu-item "Test grammar" pest-test-grammar
                  :help "Test this grammar on arbitrary input"))
    map)
  "Keymap for Pest mode.")

;;;###autoload
(define-derived-mode pest-mode prog-mode "Pest"
  "Major mode for editing Pest files.

\\{pest-mode-map}"
  (setq-local font-lock-defaults '(pest--highlights))
  (setq-local indent-line-function #'pest-indent-line)
  (setq-local imenu-prev-index-position-function #'pest-imenu-prev-index-position)
  (setq-local imenu-extract-index-name-function #'pest-imenu-extract-index-name)
  (add-hook 'flymake-diagnostic-functions 'pest-flymake nil t))



(defvar-local pest--lang-flymake-proc nil)
(defvar-local pest--lang-eldoc-proc nil)
(defvar-local pest--grammar-buffer nil)
(defvar-local pest--selected-rule nil)

(defun pest-select-rule ()
  "Select a rule for furthur analysis."
  (interactive)
  (when (null pest--grammar-buffer)
    (error "This buffer is not associated with a Pest grammar!"))
  (let* ((rules (pest--rule-list pest--grammar-buffer))
         (rule (completing-read "Start rule: " rules nil t)))
    (setq-local pest--selected-rule rule)))

(defun pest-analyze-input ()
  "Analyze the input and show a report of the parsing result in a new buffer."
  (interactive)
  (message "todo; pest-analyze-input invoked"))

(defun pest-input-flymake (report-fn &rest _args)
  "Check and give diagnosis messages about the input."
  (unless (executable-find "pesta")
    (error "Cannot find a suitable `pesta' executable"))
  (when (process-live-p pest--lang-flymake-proc)
    (kill-process pest--lang-flymake-proc))
  (if (null pest--selected-rule)
      (message "You haven't selected a rule to start; do so with `pest-select-rule'.")
    (let ((source (current-buffer)))
      (save-restriction
        (widen)
        (setq pest--lang-flymake-proc
              (make-process
               :name "pest-input-flymake"
               :noquery t
               :connection-type 'pipe
               :buffer (generate-new-buffer " *pest-input-flymake*")
               :command `("pesta" "lang_check" ,pest--selected-rule)
               :sentinel
               (lambda (proc _event)
                 (when (eq 'exit (process-status proc))
                   (unwind-protect
                       (if (with-current-buffer source (eq proc pest--lang-flymake-proc))
                           (with-current-buffer (process-buffer proc)
                             (goto-char (point-min))
                             (cl-loop
                              while (search-forward-regexp pest--diagnosis-regexp
                                                           nil t)
                              for beg = (string-to-number (match-string 1))
                              for end = (string-to-number (match-string 2))
                              for msg = (match-string 3)
                              for type = :error
                              do (message "Pest Error: %s" beg end msg)
                              collect (flymake-make-diagnostic source
                                                               beg
                                                               end
                                                               type
                                                               msg)
                              into diags
                              finally (funcall report-fn diags)))
                         (flymake-log :warning "Canceling obsolete check %s"
                                      proc))
                     (kill-buffer (process-buffer proc)))))))
        (let* ((grammar (with-current-buffer pest--grammar-buffer
                          (save-restriction
                            (widen)
                            (buffer-string))))
               (input (buffer-string))
               (send-data (json-encode-list (list grammar input))))
          (process-send-string pest--lang-flymake-proc send-data)
          (process-send-eof pest--lang-flymake-proc))))))

(defvar pest-input-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Pest")))
    (define-key map (kbd "C-c C-c") 'pest-analyze-input)
    (define-key map (kbd "C-c C-r") 'pest-select-rule)
    map))

(define-derived-mode pest-input-mode prog-mode "Pest-Input"
  "Major mode for input to test a Pest grammar file.  This mode should only be enabled with `pest-test-grammar'.

\\{pest-input-mode-map}"
  (add-hook 'flymake-diagnostic-functions 'pest-input-flymake nil t)
  (flymake-mode))

(provide 'pest-mode)
