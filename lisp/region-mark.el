;;; region-mark.el --- Obtain pointers of a selected region  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ksqsf

;; Author: ksqsf <i@ksqsf.moe>
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

;; When hacking Emacs, I often feel the need to use a portion of
;; buffer text for testing.  This package fulfills this need.

;;; Code:

(defvar-local rm-overlay nil
  "The overlay used by region-mark.")
(defvar-local rm-left nil
  "The pointer to the beginning of the marked region.

`nil' when no region is marked.")
(defvar-local rm-right nil
  "The pointer to the end of the marked region.

`nil' when no region is marked.")

;;;###autoload
(defun rm-set (beg end)
  "Mark a region for quick references to both pointers of the region.

Only one region can be marked in a buffer."
  (interactive "r")
  (unless rm-overlay
    (rm-clear))
  (setq rm-overlay (make-overlay beg end))
  (setq rm-left beg)
  (setq rm-right end)
  (overlay-put rm-overlay 'face 'highlight))

;;;###autoload
(defun rm-clear ()
  "Clear the marked region."
  (interactive)
  (when rm-overlay
    (delete-overlay rm-overlay)
    (setq rm-overlay nil
          rm-left nil
          rm-right nil)))

(provide 'region-mark)
;;; region-mark.el ends here
