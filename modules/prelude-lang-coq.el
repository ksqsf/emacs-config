;;; -*- lexical-binding: t; -*-

(use-package proof-general
  :commands (coq-mode)
  :hook ((coq-mode . yas-minor-mode))
  :mode ("\\.v\\'" . coq-mode)
  :custom
  (proof-splash-enable nil))

(use-package company-coq
  :hook (coq-mode . company-coq-mode)
  (setq company-coq-live-on-the-edge t))


;;
;; The following code is historic.
;;

;; Resolve Coq and Verilog
(defvar coq-or-verilog-mode--regexp "\\(?:\\(Theorem\\|Ltac\\|Example\\|Lemma\\|Axiom\\)[ 	]\\).+:")

(defun coq-or-verilog-mode ()
  "Analyze buffer and enable either Coq or Verilog mode.

Coq uses .v extension for Coq files, which is also the one used
for Verilog files.  This makes matching on file name insufficient
for detecting major mode that should be used.

This function attempts to use file contents to determine whether
the code is Coq or Verilog and based on that chooses whether to
enable `coq-mode' or `verilog-mode'.

The testing procedure and criteria are not sufficiently enough,
but works well enough for distinguishing source files in Software
Foundations and typical Verilog files."
  (if (save-excursion
        (save-restriction
          (save-match-data
            (widen)
            (goto-char (point-min))
            (re-search-forward coq-or-verilog-mode--regexp
                               (point-max) t))))
      (coq-mode)
    (verilog-mode)))

;; I don't write Verilog anymore!!
;; (add-to-list 'auto-mode-alist '("\\.v\\'" . coq-or-verilog-mode))

(provide 'prelude-lang-coq)
