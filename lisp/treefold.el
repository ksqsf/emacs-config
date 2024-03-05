;;; treefold.el --- Fold tree-format texts as if it is a tree -*- lexical-binding: t; -*-

;; Author: ksqsf <i@ksqsf.moe>
;; URL: https://github.com/ksqsf/emacs-config
;; Version: 0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: convenience

;; Copyright (C) 2020 ksqsf

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

;; treefold.el provides a minor mode, which contains commands to help
;; you fold any text that looks like a tree, e.g. the output of
;; tree(1) or that of cargo-tree.

;; For instance, the output of tree(1):

;; .
;; ├── Cargo.lock
;; ├── Cargo.toml
;; ├── deps
;; └── src
;;     ├── errors.rs
;;     └── main.rs

;; If you press Return on the line "src", you will fold the
;; contents of "src", and get the result:

;; .
;; ├── Cargo.lock
;; ├── Cargo.toml
;; ├── deps
;; └── src [...]

;;; Code:

(setq treefold-continuation-regexp (rx (or " " " " "|" "│")))
(setq treefold-subtree-indicator-regexp (rx (or "+" "└" "├" "|-")))

(defun treefold--subtree-end ()
  (save-excursion
    (let (end (col (current-column)))
      (while (and (= (forward-line) 0)
                  (= (move-to-column col) col)
                  (looking-at treefold-continuation-regexp))
        nil)
      (1+ (point-at-eol 0)))))

(defun treefold--folded-line (start end)
  (save-excursion
    (goto-char start)
    (format "%s [...]\n" (buffer-substring start (point-at-eol)))))

(defun treefold--fold-subtree (start end)
  "Fold a subtree, which spans region from START to END."
  (save-excursion
    (goto-char start)
    (let* ((eol (point-at-eol))
           (line-overlay (make-overlay start eol))
           (hide-overlay (make-overlay (1+ eol) end)))
      (if (>= (1+ eol) end)
          ;; There's nothing to hide.
          (progn
            (delete-overlay line-overlay)
            (delete-overlay hide-overlay))
        (overlay-put line-overlay 'after-string " [...]")
        (overlay-put line-overlay 'evaporate t)
        (overlay-put line-overlay 'treefold 'line)
        (overlay-put line-overlay 'treefold-link hide-overlay)
        (overlay-put hide-overlay 'invisible t)
        (overlay-put line-overlay 'evaporate t)
        (overlay-put hide-overlay 'treefold 'hide)
        (overlay-put hide-overlay 'treefold-link line-overlay)))))

(defun treefold--unfold-in-region (start end)
  "Unfold all subtrees in region (START, END)."
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'treefold)
      (delete-overlay overlay))))

(defun treefold--fold ()
  "Fold the subtree found on this line."
  (beginning-of-line)
  (unless (re-search-forward treefold-subtree-indicator-regexp
                             (point-at-eol) t 1)
    (error "This line doesn't have a subtree."))
  (let* ((start (point-at-bol))
         (end (treefold--subtree-end)))
    (treefold--unfold-in-region start end)
    (treefold--fold-subtree start end)))

(defun treefold--line-overlay (pos)
  "Return the line overlay at POS, or nil if none is found."
  (catch 'return
    (dolist (overlay (overlays-at (point)))
      (when (eq 'line (overlay-get overlay 'treefold))
        (throw 'return overlay)))
    (throw 'return nil)))

;;;###autoload
(defun treefold-toggle ()
  "Toggle whether the current subtree is folded."
  (interactive)
  (save-excursion
    (let ((line-overlay (treefold--line-overlay (point))))
      (if line-overlay
          (treefold--unfold line-overlay)
        (treefold--fold)))))

(defun treefold--unfold (line-overlay)
  "Unfold the subtree found on this line."
  ;; Delete hide-overlay.
  (delete-overlay (overlay-get line-overlay 'treefold-link))
  ;; Delete line-overlay.
  (delete-overlay line-overlay))

;;;###autoload
(defun treefold-unfold-all ()
  "Unfold all subtrees in this buffer."
  (interactive)
  (treefold--unfold-in-region (point-min) (point-max)))

;;;###autoload
(defun treefold-forward-subtree (&optional n)
  "Go to the position of the indicator for the next N-th subtree.

If N is negative, search backwards."
  (interactive)
  (re-search-forward treefold-subtree-indicator-regexp nil nil (or n 1)))

;;;###autoload
(defun treefold-backward-subtree (&optional n)
  "Go to the position of the indicator for the previous N-th subtree.

If N is negative, search forward."
  (interactive)
  (treefold-forward-subtree (- (or n 1))))

;;;###autoload
(define-minor-mode treefold-mode
  "Enable treefold functions in this buffer."
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "<RET>") #'treefold-toggle)
            (define-key keymap (kbd "C-c C-n") #'treefold-forward-subtree)
            (define-key keymap (kbd "C-c C-p") #'treefold-backward-subtree)
            keymap))

(provide 'treefold)

;;; treefold.el ends here.
