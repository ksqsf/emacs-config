;; org-xlatex.el --- instant LaTeX preview in an xwidget  -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides a minor mode `org-xlatex-mode'.  It provides
;; almost instant LaTeX previewing in Org buffers by embedding MathJax
;; in an xwidget inside a child frame.  The child frame automatically
;; appears and renders the formula at the point.

;; org-xlatex is self-contained.  It does not require any external
;; programs.

;;; Code:

(require 'org)
(require 'xwidget)

(eval-and-compile
  (unless (featurep 'xwidget-internal)
    (error "Your Emacs was not built with Xwidget support")))

(defvar org-xlatex-frame nil
  "The child frame used by org-xlatex.")

(defvar org-xlatex-xwidget nil
  "The xwidget used by org-xlatex.")

(defvar org-xlatex-width 400)
(defvar org-xlatex-height 200)
(defvar org-xlatex-timer nil)
(defconst org-xlatex-html-uri (concat "file://" (expand-file-name "org-xlatex.html" (file-name-directory (or load-file-name buffer-file-name)))))

(define-minor-mode org-xlatex-mode
  "Toggle org-xlatex-mode.
Interactively with no argument, this command tggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When org-xlatex-mode is enabled, a child frame appears with the
preview of the mathematical formula (LaTeX math formula) whenevr
the point is at a formula."
  :init-value nil
  :lighter " XLaTeX"
  :group 'org-xlatex
  (if org-xlatex-mode
      (org-xlatex--setup)
    (org-xlatex--teardown)))

(defun org-xlatex--setup ()
  "Arrange the org-xlatex frame to be displayed when the point enters LaTeX fragments or environments."
  (org-xlatex--cleanup)
  (add-hook 'after-delete-frame-functions 'org-xlatex--after-delete-frame-function)
  (setq org-xlatex-timer (run-with-idle-timer 0.1 'repeat 'org-xlatex--timer-function)))

(defun org-xlatex--teardown ()
  "Disable hooks and timers set up by org-xlatex."
  (cancel-timer org-xlatex-timer)
  (setq org-xlatex-timer nil)
  (remove-hook 'after-delete-frame-functions 'org-xlatex--after-delete-frame-function)
  (org-xlatex--cleanup))

(defun org-xlatex--timer-function (&rest args)
  "Preview at point if the point is at a math formula."
  (if (and (derived-mode-p 'org-mode) (texmathp))
      (org-xlatex-preview)
    (org-xlatex--hide)))

(defun org-xlatex--after-delete-frame-function (frame)
  "Check if the newly deleted frame was org-xlatex."
  (when (eq frame org-xlatex-frame)
    (org-xlatex--cleanup)
    (setq org-xlatex-frame nil)))

(defun org-xlatex--ensure-frame ()
  "Get the current org-xlatex-frame; initialize one if it does not exist."
  (if (and org-xlatex-frame (frame-live-p org-xlatex-frame))
      org-xlatex-frame
    (org-xlatex--cleanup)
    (setq org-xlatex-frame (make-frame '((visibility . t)
                                         (undecorated . t)
                                         (no-accept-focus . t)
                                         (minibuffer . nil))))
    (with-selected-frame org-xlatex-frame
      (delete-other-windows)
      (switch-to-buffer " *org-xlatex*")
      (setq mode-line-format nil)
      (insert " ")
      (setq org-xlatex-xwidget (xwidget-insert (point-min) 'webkit "org-xlatex" org-xlatex-width org-xlatex-height))
      (xwidget-webkit-goto-uri org-xlatex-xwidget org-xlatex-html-uri))
    org-xlatex-frame))

(defun org-xlatex--cleanup ()
  "Release resources used by org-xlatex."
  (when (buffer-live-p " *org-xlatex*")
    (with-current-buffer " *org-xlatex*"
      (erase-buffer))
    (kill-buffer " *org-xlatex*"))
  (when (and org-xlatex-xwidget (xwidget-live-p org-xlatex-xwidget))
    (kill-xwidget org-xlatex-xwidget)
    (setq org-xlatex-xwidget nil))
  (when (and org-xlatex-frame (frame-live-p org-xlatex-frame))
    (delete-frame org-xlatex-frame)
    (setq org-xlatex-frame nil)))

(defun org-xlatex--latex-at-point ()
  "Obtain the contents of the LaTeX fragment or environment at point."
  (let ((context (org-element-context)))
    (when (or (eq 'latex-fragment (org-element-type context))
              (eq 'latex-environment (org-element-type context)))
      (buffer-substring-no-properties
       (org-element-property :begin context)
       (- (org-element-property :end context)
	  (org-element-property :post-blank context))))))

(defun test-html ()
  (interactive)
  (xwidget-webkit-browse-url org-xlatex-html-uri))

(defun org-xlatex--escape (latex)
  "Escape LaTeX code so that it can be used as JS strings."
  (string-replace "\n" " " (string-replace "'" "\\'" (string-replace "\\" "\\\\" latex))))

(defun org-xlatex--build-js (latex)
  "Build js code to rewrite the .latex-contents div to hold the latex code."
  (let ((template "oxlTypeset('%s');"))
    (format template (org-xlatex--escape latex))))

(defun org-xlatex--expose (parent-frame)
  "Expose the child frame."
  (set-frame-parameter org-xlatex-frame 'parent-frame parent-frame)
  (make-frame-visible org-xlatex-frame)
  (select-frame parent-frame)
  (set-frame-size org-xlatex-frame org-xlatex-width org-xlatex-height t)
  ;; todo: find the correct screen position of the point
  (with-selected-frame parent-frame
    (let* ((context (org-element-context))
           (latex-beg (org-element-property :begin context))
           (latex-end (- (org-element-property :end context)
                         (org-element-property :post-blank context)))
           (latex-beg-posn (posn-at-point latex-beg))
           (latex-beg-x (car (posn-x-y latex-beg-posn)))
           (latex-end-posn (posn-at-point latex-end))
           (latex-end-y (or (cdr (posn-x-y latex-end-posn))
                            (cdr (posn-x-y (posn-at-point)))))
           (y (+ 40 latex-end-y)))
      (set-frame-position org-xlatex-frame latex-beg-x y))))

(defun org-xlatex--hide ()
  (when (and org-xlatex-frame (frame-live-p org-xlatex-frame))
    (make-frame-invisible org-xlatex-frame)))

(defun org-xlatex--update (latex)
  (org-xlatex--ensure-frame)
  (setq org-xlatex-last-latex latex)
  (setq org-xlatex-last-js (org-xlatex--build-js latex))
  (xwidget-webkit-execute-script org-xlatex-xwidget org-xlatex-last-js))

(defun org-xlatex-preview ()
  "Preview the LaTeX formula inside a child frame at the point."
  (interactive)
  (setq org-xlatex-last-frame (selected-frame))
  (org-xlatex--ensure-frame)
  (let ((latex (org-xlatex--latex-at-point)))
    (when latex
      (org-xlatex--update latex)
      (org-xlatex--expose org-xlatex-last-frame)
      (select-frame org-xlatex-last-frame))))

(provide 'org-xlatex)
;;; org-xlatex.el ends here
