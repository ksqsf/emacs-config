;;; clipboard.el --- Clipboard utilities             -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ksqsf

;; Author: ksqsf <i@ksqsf.moe>
;; Keywords: tools
;; Package-Requires: ((emacs "28.1"))

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

;; 

;;; Code:

(defun clipboard--type-is-image (type)
  "Check whether TYPE indicates an image."
  (let* ((maybe-format (string-trim-left (symbol-name type) "image/"))
         (maybe-format (intern maybe-format)))
    (and (member maybe-format image-types)
         (image-type-available-p maybe-format))))

(defun clipboard--type-image-format (type)
  "Get the image format from TYPE.

Only meaningful when TYPE satisfies `clipboard--type-is-image'."
  (intern (string-trim-left (symbol-name type) "image/")))

(defun clipboard--insert (data type)
  (cond ((clipboard--type-is-image type)
         (insert-image (list 'image
                             :data data
                             :type (clipboard--type-image-format type))))
        (t
         (insert (format "%s" data)))))

;;;###autoload
(defun clipboard-inspect ()
  (interactive)
  (catch 'foo
    (let ((buf (get-buffer-create "*clipboard*"))
          (targets (gui-get-selection 'CLIPBOARD 'TARGETS)))
      (unless (vectorp targets)
        (message "Clipboard does not have TARGETS info.")
        (throw 'foo nil))
      (with-current-buffer buf
        (read-only-mode -1)
        (widen)
        (delete-region (point-min) (point-max))
        (mapc (lambda (target)
                (insert "\n")
                (insert (propertize (symbol-name target) 'face 'bold))
                (insert "\n")
                (clipboard--insert (gui-get-selection 'CLIPBOARD target) target)
                (insert "\n\n"))
              targets))
      (pop-to-buffer buf)
      (read-only-mode 1)
      (beginning-of-buffer))))

(provide 'clipboard)
;;; clipboard.el ends here
