(defun binary-search (&optional needle)
  "Locate for insertion in an ordered manner."
  (interactive "sNeedle: ")
  (cl-flet ((midpoint (a b)
              (+ a (/ (- b a) 2)))
            (peek (line)
              (goto-line line)
              (beginning-of-line)
              (word-at-point)))
    (let* ((a (line-number-at-pos))
           (b (save-excursion (end-of-buffer) (line-number-at-pos))))
      (while (> (- b a) 1)
        (let ((mid (midpoint a b)))
          (if (string< needle (peek mid))
              (setq b mid)
            (setq a mid))))
      (goto-line a))))

(defun gen-zrmdb-1 ()
  (interactive)
  (erase-buffer)
  (insert-file "~/Library/Rime/moran.chars.dict.yaml")
  (keep-lines "^.	[a-z]+;[a-z][a-z].*$")
  (replace-regexp "^\\(.\\)	[a-z]+;\\([a-z][a-z]\\).*$" "\\1 \\2")
  (mark-whole-buffer)
  ;;(sort-lines nil (point-min) (point-max))
  (goto-char (point-min))
  ;;(insert-file "~/Library/Rime/tools/data/zrmdboverride.txt")
  (sort-fields 1 (point-min) (point-max))
  (delete-duplicate-lines (point-min) (point-max)))

(defun gen-zrmdb ()
  (interactive)
  (find-file-other-window "~/Library/Rime/tools/data/zrmdb.txt"))

(defun make-zrmdb-table ()
  (save-window-excursion
    (let ((table (make-hash-table :test 'equal)))
      (find-file "~/Library/Rime/moran.chars.dict.yaml")
      (goto-char (point-min))
      (cl-loop while (re-search-forward "^\\(.\\)	\\([a-z][a-z]\\);\\([a-z][a-z]\\)" nil t)
               for zi = (match-string-no-properties 1)
               for code = (match-string-no-properties 3)
               for existing-codes = (gethash zi table)
               unless (member code existing-codes)
               do (puthash zi (cons code existing-codes) table))
      table)))

(defun make-essay-table (&optional path)
  (setq path (or path "/Library/Input Methods/Squirrel.app/Contents/SharedSupport/essay.txt"))
  (let ((table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (cl-loop while (re-search-forward "\\(\\cc+\\)	\\([0-9]+\\)" nil t)
               for word = (match-string-no-properties 1)
               for freq = (string-to-number (match-string-no-properties 2))
               do (puthash word freq table)))
    table))

(defun make-code-word-table ()
  "Make code-word table based on the current buffer.

CAR is the first candidate, etc."
  (save-excursion
    (let ((table (make-hash-table :test 'equal)))
      (goto-char (point-max))
      (cl-loop while (re-search-backward "^\\([a-z]+\\)	\\(.+\\)$" nil t)
               for code = (match-string-no-properties 1)
               for word = (match-string-no-properties 2)
               for existing-words = (gethash code table)
               do (puthash code (cons word existing-words) table))
      table)))

(defun sort-with-freq-table (beg end)
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (let ((current-table (make-code-word-table))
          (essay-table (make-essay-table)))
      (delete-region beg end)
      (cl-loop for code being the hash-keys of current-table
               for words = (gethash code current-table)
               for sorted-words = (sort words (lambda (a b)
                                                (> (gethash a essay-table 0)
                                                   (gethash b essay-table 0))))
               do (cl-loop for word in sorted-words
                           do (insert code "\t" word "\n"))))
    (sort-fields 1 (point-min) (point-max))))

(defun yedict-char-at-point ()
  (interactive)
  (let ((char (char-after)))
    (shell-command (format "open 'http://yedict.com/zscontent.asp?uni=%X'" char))))

(defun yedict-char-bol ()
  (interactive)
  (let ((char (char-after (pos-bol))))
    (shell-command (format "open 'http://yedict.com/zscontent.asp?uni=%X'" char))))

(defvar-local yedict-follow-mode-last-line-number nil)
(defvar-local yedict-follow-mode-xwidget-buffer nil)

(define-minor-mode yedict-follow-mode
  "Let yedict follow the current line."
  :global nil
  (if yedict-follow-mode
      (progn
        (add-hook 'post-command-hook 'yedict-follow-mode-watch-line-changed nil t))
    (remove-hook 'post-command-hook 'yedict-follow-mode-watch-line-changed t)))

(defun yedict-follow-mode-watch-line-changed ()
  (when (not (equal (line-number-at-pos) yedict-follow-mode-last-line-number))
    (yedict-follow-mode-navigate)
    (setq yedict-follow-mode-last-line-number (line-number-at-pos))))

(defmacro with-yedict-xwidget-buffer (&rest body)
  `(progn
     (unless yedict-follow-mode-xwidget-buffer
       (setq yedict-follow-mode-xwidget-buffer (xwidget-webkit--create-new-session-buffer "about:blank")))
     (with-current-buffer yedict-follow-mode-xwidget-buffer
       ,@body)))

(defun yedict-follow-mode-navigate ()
  (let ((char (char-after (pos-bol))))
    (with-yedict-xwidget-buffer
     (message "Display info about %c" char)
     (xwidget-webkit-goto-uri (xwidget-webkit-current-session) (format "http://yedict.com/zscontent.asp?uni=%X" char)))))
