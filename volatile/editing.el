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

;; Maintain the tables of table-based Chinese input methods.
;;
;; Typical usage
;; =============
;;
;; Extract all Chinese words:
;;    M-x extract-regexp RET \cc+
;; Transpose table format (exchange the first and second columns):
;;    M-x transpose-table-singular
;;
;; Code
;; ====
;;
;; Use `with-each-code-word' and `with-each-word-code' to iterate over
;; the default table.

(defun extract-regexp (regexp)
  "Search for REGEXP in the current buffer and extract all matches.

Each match will be placed on a separate line."
  (interactive "sRegexp: ")
  (with-current-buffer (get-buffer-create "*extract*")
    (erase-buffer))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((matched (match-string 0)))
        (with-current-buffer "*extract*"
          (insert matched "\n")))))
  (display-buffer "*extract*"))

(defun transpose-table-singular ()
  "Exchange the first and the second columns of the current buffer."
  (interactive)
  (while (re-search-forward (rx bol (group (0+ any)) "\t" (group (0+ any)) eol) nil t)
    (replace-match "\\2\t\\1")))

;; (defun transpose-table-multiple ()
;;   (interactive)
;;   (while (re-search-forward (rx bol (group (0+ any)) "\t" (group (0+ any)) eol) nil t)
;;     (let ((first (match-string 1))
;;           (second (s-split " " (match-string 2))))
;;       (replace-match "")
;;       (dolist (it second)
;;         (insert (format "%s\t%s\n" it first))))))

(defmacro with-each-word-code (word-symbol code-symbol &rest body)
  (declare (indent 2))
  `(while (re-search-forward "^\\(\\cc+\\)\t\\([a-z]+\\)" nil t)
     (let ((,word-symbol (match-string 1))
           (,code-symbol (match-string 2)))
       ,@body)))

(defmacro with-each-code-word (code-symbol word-symbol &rest body)
  (declare (indent 2))
  `(while (re-search-forward "^\\([a-z]+\\)\t\\(\\cc+\\)" nil t)
     (let ((,code-symbol (match-string 1))
           (,word-symbol (match-string 2)))
       ,@body)))

(defun all-codes-satisfying (file pred)
  (save-window-excursion
    (find-file file)
    (goto-char (point-min))
    (let (result)
      (with-each-word-code word code
        (when (funcall pred word code)
          (push code result)))
      result)))

(defun keep-empty-3code-words ()
  (interactive)
  (keep-lines "^[a-z][a-z][a-z]\t\\cc\\cc")
  (let ((non-empty-3code (all-codes-satisfying
                          "~/Library/Rime/moran_fixed.dict.yaml"
                          (lambda (word code)
                            (= (length code) 3)))))
    (with-each-code-word code word
      (when (member code non-empty-3code)
        (delete-line)))))

(defun make-hash-set ()
  (make-hash-table :test 'equal))

(defun hash-set-add (set elem)
  (puthash elem t set))

(defun hash-set-del (set elem)
  (remhash elem set))

(defun hash-set-in (set elem)
  (gethash elem set nil))

(defun hash-set-union (set1 set2)
  "Find set1 ∪ set2."
  (let ((result (make-hash-set)))
    (maphash (lambda (key _)
               (hash-set-add result key))
             set1)
    (maphash (lambda (key _)
               (hash-set-add result key))
             set2)))

(defun hash-set-intersection (set1 set2)
  "Find set1 ∩ set2."
  (let ((result (make-hash-set)))
    (maphash (lambda (key _)
               (when (hash-set-in set2 key)
                 (hash-set-add result key)))
             set1)
    result))

(defun hash-set-difference (set1 set2)
  "Find set1 - set2."
  (let ((result (make-hash-set)))
    (maphash (lambda (key _)
               (when (not (hash-set-in set2 key))
                 (hash-set-add result key)))
             set1)
    result))

(defun construct-buffer-set (buf)
  (with-current-buffer buf
    (let ((set (make-hash-table :test 'equal)))
      (goto-char (point-min))
      (while (not (eobp))
        (puthash (buffer-substring-no-properties (pos-bol) (pos-eol)) t set)
        (forward-line 1))
      set)))

(defun hash-set-insert (set)
  (maphash (lambda (key _)
             (insert key "\n"))
           set))

(defun compare-buffers-as-sets (buf-a buf-b)
  "Compare two buffers as sets, with lines as elements.

The results are stored in three buffers: 
 *compare:intersection*
 *compare:only-in-a*
 *compare:only-in-b*"
  (interactive
   (list
    (get-buffer (read-buffer "buffer-a " (current-buffer) t))
    (get-buffer
     (read-buffer "buffer-b "
		  (window-buffer (next-window)) t))))
  (let ((set1 (construct-buffer-set buf-a))
        (set2 (construct-buffer-set buf-b)))
    (with-current-buffer (get-buffer-create "*compare:intersection*")
      (hash-set-insert (hash-set-intersection set1 set2)))
    (with-current-buffer (get-buffer-create "*compare:only-in-a*")
      (hash-set-insert (hash-set-difference set1 set2)))
    (with-current-buffer (get-buffer-create "*compare:only-in-b*")
      (hash-set-insert (hash-set-difference set2 set1)))))
