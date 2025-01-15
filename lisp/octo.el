;;; octo.el --- Multi-root window management in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  ksqsf

;; Author: ksqsf
;; Keywords: convenience

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

;; octo.el is an opinionated, experimental library for multi-root
;; window management.  With octo.el, you can:
;;
;; - Create "gen"-windows.  They are virtual root windows where
;;   window-management commands are effective.
;;
;; - Focus on one gen window, so that window commands ONLY affect
;;   windows in the currently focused gen window.
;;
;; * Usage
;;
;; 1) Activate `octo-mode'.
;;
;; 2) Create your top-level layout, then call `octo-activate'.
;;
;; At this point, each child (internal or not) of the root window will
;; be a Gen window. And, for example, `C-x 1' will be confined to the
;; currently selected Gen window.

;;; Code:

(defun octo--set-gen (win flag)
  "Mark whether window WIN is a gen according to FLAG."
  (set-window-parameter win 'octo-is-gen flag))

(defun octo--gen-p (win)
  "Check if window WIN is a gen window."
  (window-parameter win 'octo-is-gen))

(defun octo--gen (&optional win)
  "Find the gen window of window WIN, or the selected window if nil."
  (cond
   ((eq win nil) (octo--gen (selected-window)))
   ((frame-root-window-p win) win)
   ((octo--gen-p win) win)
   (t (octo--gen (window-parent win)))))

(defun octo-delete-windows ()
  "Delete all windows belong to the current gen."
  (interactive)
  (delete-window (octo--gen)))

(defun octo-split-window-1 (fun)
  "Split window while maintaining GEN flags."
  ;; Always create a Gen internal window when splitting a Gen leaf window.
  (let* ((win (selected-window))
         (win-is-gen (octo--gen-p win))
         (window-combination-limit (or win-is-gen window-combination-limit))
         (oldp (window-parent win))
         (new (funcall fun))
         (newp (window-parent win)))
    ;; If a new parent window is installed, transfer the GEN flag to it.
    (unless (eq oldp newp)
      (octo--set-gen newp (octo--gen-p win))
      (octo--set-gen win nil))
    ;; Return the new window.
    new))

(defun octo-split-window-below ()
  "Split window below while maintaining GEN flags."
  (interactive)
  (octo-split-window-1 #'split-window-below))

(defun octo-split-window-right ()
  "Split window right while maintaining GEN flags."
  (interactive)
  (octo-split-window-1 #'split-window-right))

(defun octo-delete-window ()
  "Delete window while maintaing GEN flags."
  (interactive)
  ;; Deletion of a leaf window may result in deletion of its parent
  ;; internal window.
  (let* ((win (selected-window))
         (parent (window-parent win)))
    (when (<= (window-child-count parent) 2)
      (let ((sibling (or (window-next-sibling win)
                         (window-prev-sibling win))))
        (octo--set-gen sibling (octo--gen-p parent))))
    (delete-window win)))

(defun octo-delete-other-windows ()
  "Mark the current window fill its gen."
  (interactive)
  (let* ((win (selected-window))
         (gen (octo--gen win)))
    (cond
     ((eq win gen)
      (message "No other windows to delete."))
     (t
      (delete-other-windows-internal win gen)
      (octo--set-gen win t)))))

(defun octo-deactivate ()
  "Clear all GEN flags."
  (interactive)
  (dolist (win (window-list))
    (octo--set-gen win nil)))

(defun octo-activate ()
  "Set each child window of the root window to be a new gen window."
  (interactive)
  (octo-deactivate)
  (let* ((root (frame-root-window))
         (cur (window-child root)))
    (while cur
      (octo--set-gen cur t)
      (setq cur (window-next-sibling cur)))))

(defvar octo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x 0") 'octo-delete-window)
    (define-key map (kbd "C-x 1") 'octo-delete-other-windows)
    (define-key map (kbd "C-x 2") 'octo-split-window-below)
    (define-key map (kbd "C-x 3") 'octo-split-window-right)
    map)
  "Keymap for `octo-mode`.")

;;;###autoload
(define-minor-mode octo-mode
  "A global minor mode with custom window management key bindings."
  :lighter " Octo"
  :keymap octo-mode-map
  :global t)

(provide 'octo)
;;; octo.el ends here
