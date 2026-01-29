;;; -*- lexical-binding: t; -*-

(require 'tramp)
(require 'tramp-sh)

(defconst tramp-asyncssh-method "asyncssh"
  "Asynchronous SSH method for Tramp.")

(add-to-list 'tramp-methods
             `(,tramp-asyncssh-method
               (tramp-login-program "ssh")
               (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c")
                                  ("-e" "none") ("-t" "-t") ("%h")))
               (tramp-async-args (("-q")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-login ("-l"))
               (tramp-remote-shell-args ("-c"))
               (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null")
                              ("-o" "UserKnownHostsFile=/dev/null")
                              ("-o" "StrictHostKeyChecking=no")))
               (tramp-default-port 22)))

(defvar tramp-asyncssh--connections (make-hash-table :test 'equal)
  "Hash table of active async SSH connections.")

(defun tramp-asyncssh--connection-key (vec)
  "Generate a connection key from Tramp vector VEC."
  (format "%s@%s:%s"
          (tramp-file-name-user vec)
          (tramp-file-name-host vec)
          (or (tramp-file-name-port vec) 22)))

(defun tramp-asyncssh--get-connection (vec)
  "Get or create an async SSH connection for VEC."
  (let* ((key (tramp-asyncssh--connection-key vec))
         (conn (gethash key tramp-asyncssh--connections)))
    (if (and conn (process-live-p (plist-get conn :process)))
        conn
      ;; Create new connection
      (let* ((process-name (format "asyncssh-%s" key))
             (buffer (generate-new-buffer (format " *%s*" process-name)))
             (login-args (list "ssh"
                              "-l" (tramp-file-name-user vec)
                              "-p" (number-to-string 
                                    (or (tramp-file-name-port vec) 22))
                              "-o" "ControlMaster=auto"
                              "-o" "ControlPath=/tmp/tramp-asyncssh-%r@%h:%p"
                              "-o" "ControlPersist=600"
                              (tramp-file-name-host vec)))
             (proc (apply #'start-process process-name buffer login-args))
             (connection (list :process proc
                             :buffer buffer
                             :key key
                             :pending-commands nil
                             :mutex (make-mutex "asyncssh-conn"))))
        
        ;; Wait for connection to be ready
        (accept-process-output proc 2)
        
        ;; Store connection
        (puthash key connection tramp-asyncssh--connections)
        connection))))

(defun tramp-asyncssh--close-connection (vec)
  "Close async SSH connection for VEC."
  (let* ((key (tramp-asyncssh--connection-key vec))
         (conn (gethash key tramp-asyncssh--connections)))
    (when conn
      (let ((proc (plist-get conn :process))
            (buf (plist-get conn :buffer)))
        (when (process-live-p proc)
          (delete-process proc))
        (when (buffer-live-p buf)
          (kill-buffer buf))
        (remhash key tramp-asyncssh--connections)))))

(defun tramp-asyncssh--execute-command (vec command)
  "Execute COMMAND on remote host specified by VEC asynchronously.
Returns a promise that resolves to the command output."
  (promise-create
   (lambda (resolve reject)
     (let* ((conn (tramp-asyncssh--get-connection vec))
            (proc (plist-get conn :process))
            (buf (plist-get conn :buffer))
            (marker (format "ASYNCSSH_CMD_%d_" (random 1000000)))
            (start-marker (format "%sSTART\n" marker))
            (end-marker (format "%sEND" marker))
            (full-command (format "echo '%s'; %s; echo $? '%s'\n"
                                start-marker
                                command
                                end-marker))
            (output "")
            (found-start nil)
            (filter-fn nil))
       
       (setq filter-fn
             (lambda (proc string)
               (setq output (concat output string))
               
               ;; Check if we've received the complete output
               (when (and (not found-start)
                         (string-match (regexp-quote start-marker) output))
                 (setq found-start t)
                 (setq output (substring output (match-end 0))))
               
               (when (and found-start
                         (string-match (concat "\\([0-9]+\\) "
                                             (regexp-quote end-marker))
                                     output))
                 (let* ((exit-code (string-to-number (match-string 1 output)))
                        (cmd-output (substring output 0 (match-beginning 0))))
                   ;; Remove filter
                   (set-process-filter proc nil)
                   
                   ;; Resolve or reject based on exit code
                   (if (= exit-code 0)
                       (funcall resolve cmd-output)
                     (funcall reject (list 'command-failed
                                         :exit-code exit-code
                                         :output cmd-output)))))))
       
       ;; Set up filter and send command
       (with-current-buffer buf
         (erase-buffer))
       (set-process-filter proc filter-fn)
       (process-send-string proc full-command)))))

(defun tramp-asyncssh--batch-execute (vec commands)
  "Execute multiple COMMANDS in batch on VEC.
Returns a promise that resolves to a list of outputs."
  (promise-create
   (lambda (resolve reject)
     (let* ((conn (tramp-asyncssh--get-connection vec))
            (proc (plist-get conn :process))
            (buf (plist-get conn :buffer))
            (marker (format "BATCH_%d_" (random 1000000)))
            (batch-script (mapconcat
                          (lambda (cmd)
                            (format "echo '%s_START'; %s; echo $? '%s_END'"
                                  marker cmd marker))
                          commands
                          "\n"))
            (output "")
            (results nil)
            (filter-fn nil))
       
       (setq filter-fn
             (lambda (proc string)
               (setq output (concat output string))
               
               ;; Parse all completed commands
               (let ((pos 0)
                     (pattern (format "%s_START\n\\(\\(?:.\\|\n\\)*?\\)\\([0-9]+\\) %s_END"
                                    marker marker)))
                 (while (string-match pattern output pos)
                   (let ((cmd-output (match-string 1 output))
                         (exit-code (string-to-number (match-string 2 output))))
                     (push (list :output cmd-output :exit-code exit-code) results)
                     (setq pos (match-end 0))))
                 
                 ;; Check if all commands completed
                 (when (= (length results) (length commands))
                   (set-process-filter proc nil)
                   (funcall resolve (nreverse results))))))
       
       (with-current-buffer buf
         (erase-buffer))
       (set-process-filter proc filter-fn)
       (process-send-string proc (concat batch-script "\n"))))))

(defun tramp-asyncssh-handle-file-exists-p (filename)
  "Check if FILENAME exists asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (command (format "test -e %s" (shell-quote-argument localname))))
    (condition-case err
        (progn
          (await (tramp-asyncssh--execute-command vec command))
          t)
      (error nil))))

(defun tramp-asyncssh-handle-file-readable-p (filename)
  "Check if FILENAME is readable asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (command (format "test -r %s" (shell-quote-argument localname))))
    (condition-case err
        (progn
          (await (tramp-asyncssh--execute-command vec command))
          t)
      (error nil))))

(defun tramp-asyncssh-handle-file-writable-p (filename)
  "Check if FILENAME is writable asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (command (format "test -w %s || test -w $(dirname %s)"
                        (shell-quote-argument localname)
                        (shell-quote-argument localname))))
    (condition-case err
        (progn
          (await (tramp-asyncssh--execute-command vec command))
          t)
      (error nil))))

(defun tramp-asyncssh-handle-file-directory-p (filename)
  "Check if FILENAME is a directory asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (command (format "test -d %s" (shell-quote-argument localname))))
    (condition-case err
        (progn
          (await (tramp-asyncssh--execute-command vec command))
          t)
      (error nil))))

(defun tramp-asyncssh-handle-file-attributes (filename &optional id-format)
  "Get attributes of FILENAME asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         ;; Use stat to get file attributes in a parseable format
         (command (format "stat -c '%%f %%h %%u %%g %%s %%Y %%X' %s 2>/dev/null || stat -f '%%p %%l %%u %%g %%z %%m %%a' %s"
                        (shell-quote-argument localname)
                        (shell-quote-argument localname))))
    (condition-case err
        (let* ((output (await (tramp-asyncssh--execute-command vec command)))
               (parts (split-string (string-trim output))))
          (when (>= (length parts) 7)
            (list
             (not (eq (logand (string-to-number (nth 0 parts) 16) #o40000) 0)) ; directory
             (string-to-number (nth 1 parts))  ; links
             (if (eq id-format 'string)
                 (format "%s" (nth 2 parts))
               (string-to-number (nth 2 parts))) ; uid
             (if (eq id-format 'string)
                 (format "%s" (nth 3 parts))
               (string-to-number (nth 3 parts))) ; gid
             nil ; atime (we'll use mtime)
             (seconds-to-time (string-to-number (nth 5 parts))) ; mtime
             (seconds-to-time (string-to-number (nth 5 parts))) ; ctime
             (string-to-number (nth 4 parts))  ; size
             (format "%o" (string-to-number (nth 0 parts) 16)) ; mode string
             nil ; gid change
             nil ; inode
             nil))) ; device
      (error nil))))

(defun tramp-asyncssh-handle-insert-file-contents
    (filename &optional visit beg end replace)
  "Insert contents of FILENAME into current buffer asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (command (if (and beg end)
                     (format "dd if=%s bs=1 skip=%d count=%d 2>/dev/null"
                           (shell-quote-argument localname)
                           beg
                           (- end beg))
                   (format "cat %s" (shell-quote-argument localname)))))
    (when replace
      (erase-buffer))
    (let ((content (await (tramp-asyncssh--execute-command vec command)))
          (start-pos (point)))
      (insert content)
      (when visit
        (setq buffer-file-name filename)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))
      (list filename (length content)))))

(defun tramp-asyncssh-handle-write-region
    (start end filename &optional append visit lockname mustbenew)
  "Write region to FILENAME asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (content (if (stringp start)
                     start
                   (buffer-substring-no-properties start end)))
         ;; Encode content for safe transmission
         (encoded (base64-encode-string content t))
         (command (format "echo '%s' | base64 -d %s %s"
                        encoded
                        (if append ">>" ">")
                        (shell-quote-argument localname))))
    (await (tramp-asyncssh--execute-command vec command))
    (when visit
      (setq buffer-file-name filename)
      (set-visited-file-modtime)
      (set-buffer-modified-p nil))
    nil))

(defun tramp-asyncssh-handle-delete-file (filename &optional trash)
  "Delete FILENAME asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (command (format "rm -f %s" (shell-quote-argument localname))))
    (await (tramp-asyncssh--execute-command vec command))
    nil))

(defun tramp-asyncssh-handle-delete-directory (directory &optional recursive trash)
  "Delete DIRECTORY asynchronously."
  (let* ((vec (tramp-dissect-file-name directory))
         (localname (tramp-file-name-localname vec))
         (command (if recursive
                     (format "rm -rf %s" (shell-quote-argument localname))
                   (format "rmdir %s" (shell-quote-argument localname)))))
    (await (tramp-asyncssh--execute-command vec command))
    nil))

(defun tramp-asyncssh-handle-make-directory (dir &optional parents)
  "Create directory DIR asynchronously."
  (let* ((vec (tramp-dissect-file-name dir))
         (localname (tramp-file-name-localname vec))
         (command (format "mkdir %s %s"
                        (if parents "-p" "")
                        (shell-quote-argument localname))))
    (await (tramp-asyncssh--execute-command vec command))
    nil))

(defun tramp-asyncssh-handle-rename-file (filename newname &optional ok-if-already-exists)
  "Rename FILENAME to NEWNAME asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
                  (newvec (tramp-dissect-file-name newname))
         (localname (tramp-file-name-localname vec))
         (newlocalname (tramp-file-name-localname newvec))
         (command (format "mv %s %s %s"
                        (if ok-if-already-exists "-f" "-n")
                        (shell-quote-argument localname)
                        (shell-quote-argument newlocalname))))
    ;; Check if both files are on the same host
    (unless (string= (tramp-asyncssh--connection-key vec)
                    (tramp-asyncssh--connection-key newvec))
      (error "Cannot rename across different hosts"))
    (await (tramp-asyncssh--execute-command vec command))
    nil))

(defun tramp-asyncssh-handle-copy-file (filename newname &optional ok-if-already-exists 
                                                 keep-date preserve-uid-gid preserve-extended-attributes)
  "Copy FILENAME to NEWNAME asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (newvec (tramp-dissect-file-name newname))
         (localname (tramp-file-name-localname vec))
         (newlocalname (tramp-file-name-localname newvec))
         (flags (concat
                (if ok-if-already-exists "-f" "")
                (if keep-date "p" "")
                (if preserve-uid-gid "p" "")))
         (command (format "cp -%s %s %s"
                        (if (string-empty-p flags) "a" flags)
                        (shell-quote-argument localname)
                        (shell-quote-argument newlocalname))))
    ;; Check if both files are on the same host
    (unless (string= (tramp-asyncssh--connection-key vec)
                    (tramp-asyncssh--connection-key newvec))
      (error "Cannot copy across different hosts"))
    (await (tramp-asyncssh--execute-command vec command))
    nil))

(defun tramp-asyncssh-handle-directory-files (directory &optional full match nosort)
  "List files in DIRECTORY asynchronously."
  (let* ((vec (tramp-dissect-file-name directory))
         (localname (tramp-file-name-localname vec))
         (command (format "ls -1a %s" (shell-quote-argument localname)))
         (output (await (tramp-asyncssh--execute-command vec command)))
         (files (split-string output "\n" t)))
    ;; Filter out . and ..
    (setq files (cl-remove-if (lambda (f) (member f '("." ".."))) files))
    ;; Apply match pattern
    (when match
      (setq files (cl-remove-if-not
                   (lambda (f) (string-match match f))
                   files)))
    ;; Make full paths if requested
    (when full
      (setq files (mapcar (lambda (f)
                           (tramp-make-tramp-file-name
                            (tramp-file-name-method vec)
                            (tramp-file-name-user vec)
                            (tramp-file-name-domain vec)
                            (tramp-file-name-host vec)
                            (tramp-file-name-port vec)
                            (concat (file-name-as-directory localname) f)))
                         files)))
    ;; Sort unless nosort
    (unless nosort
      (setq files (sort files #'string<)))
    files))

(defun tramp-asyncssh-handle-directory-files-and-attributes
    (directory &optional full match nosort id-format)
  "List files in DIRECTORY with attributes asynchronously.
This is optimized to batch all stat calls in one command."
  (let* ((vec (tramp-dissect-file-name directory))
         (localname (tramp-file-name-localname vec))
         ;; Get list of files and their attributes in one go
         (command (format "cd %s && ls -1a | while read f; do stat -c \"$f\\t%%f\\t%%h\\t%%u\\t%%g\\t%%s\\t%%Y\\t%%X\" \"$f\" 2>/dev/null || stat -f \"$f\\t%%p\\t%%l\\t%%u\\t%%g\\t%%z\\t%%m\\t%%a\" \"$f\"; done"
                        (shell-quote-argument localname)))
         (output (await (tramp-asyncssh--execute-command vec command)))
         (lines (split-string output "\n" t))
         (results nil))
    
    (dolist (line lines)
      (let* ((parts (split-string line "\t"))
             (filename (car parts))
             (attrs (cdr parts)))
        ;; Skip . and ..
        (unless (member filename '("." ".."))
          ;; Apply match pattern
          (when (or (not match) (string-match match filename))
            (when (>= (length attrs) 7)
              (let ((full-name (if full
                                 (tramp-make-tramp-file-name
                                  (tramp-file-name-method vec)
                                  (tramp-file-name-user vec)
                                  (tramp-file-name-domain vec)
                                  (tramp-file-name-host vec)
                                  (tramp-file-name-port vec)
                                  (concat (file-name-as-directory localname) filename))
                               filename))
                    (file-attrs (list
                                (not (eq (logand (string-to-number (nth 0 attrs) 16) #o40000) 0))
                                (string-to-number (nth 1 attrs))
                                (if (eq id-format 'string)
                                    (format "%s" (nth 2 attrs))
                                  (string-to-number (nth 2 attrs)))
                                (if (eq id-format 'string)
                                    (format "%s" (nth 3 attrs))
                                  (string-to-number (nth 3 attrs)))
                                nil
                                (seconds-to-time (string-to-number (nth 5 attrs)))
                                (seconds-to-time (string-to-number (nth 5 attrs)))
                                (string-to-number (nth 4 attrs))
                                (format "%o" (string-to-number (nth 0 attrs) 16))
                                nil nil nil)))
                (push (cons full-name file-attrs) results)))))))
    
    (if nosort
        (nreverse results)
      (sort results (lambda (a b) (string< (car a) (car b)))))))

(defun tramp-asyncssh-handle-file-local-copy (filename)
  "Copy FILENAME to a local temporary file asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (tmpfile (tramp-compat-make-temp-file filename))
         (command (format "cat %s" (shell-quote-argument localname)))
         (content (await (tramp-asyncssh--execute-command vec command))))
    (with-temp-file tmpfile
      (insert content))
    tmpfile))

(defun tramp-asyncssh-handle-insert-directory
    (filename switches &optional wildcard full-directory-p)
  "Insert directory listing for FILENAME asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (command (format "ls %s %s"
                        switches
                        (shell-quote-argument localname)))
         (output (await (tramp-asyncssh--execute-command vec command))))
    (insert output)))

(defun tramp-asyncssh-handle-file-name-all-completions (filename directory)
  "Return all completions for FILENAME in DIRECTORY asynchronously."
  (let* ((vec (tramp-dissect-file-name directory))
         (localname (tramp-file-name-localname vec))
         (command (format "cd %s && compgen -f %s 2>/dev/null || ls -1a | grep '^%s'"
                        (shell-quote-argument localname)
                        (shell-quote-argument filename)
                        (regexp-quote filename)))
         (output (condition-case nil
                    (await (tramp-asyncssh--execute-command vec command))
                  (error "")))
         (files (split-string output "\n" t)))
    files))

(defun tramp-asyncssh-handle-expand-file-name (name &optional dir)
  "Expand file NAME asynchronously."
  (let* ((vec (and dir (tramp-dissect-file-name dir)))
         (localname (if vec
                       (tramp-file-name-localname vec)
                     "~"))
         (expanded-name (expand-file-name name localname)))
    (if vec
        (tramp-make-tramp-file-name
         (tramp-file-name-method vec)
         (tramp-file-name-user vec)
         (tramp-file-name-domain vec)
         (tramp-file-name-host vec)
         (tramp-file-name-port vec)
         expanded-name)
      expanded-name)))

(defun tramp-asyncssh-handle-file-newer-than-file-p (file1 file2)
  "Check if FILE1 is newer than FILE2 asynchronously."
  (let* ((vec1 (tramp-dissect-file-name file1))
         (vec2 (tramp-dissect-file-name file2))
         (local1 (tramp-file-name-localname vec1))
         (local2 (tramp-file-name-localname vec2)))
    (unless (string= (tramp-asyncssh--connection-key vec1)
                    (tramp-asyncssh--connection-key vec2))
      (error "Cannot compare files on different hosts"))
    (let ((command (format "test %s -nt %s"
                          (shell-quote-argument local1)
                          (shell-quote-argument local2))))
      (condition-case nil
          (progn
            (await (tramp-asyncssh--execute-command vec1 command))
            t)
        (error nil)))))

(defun tramp-asyncssh-handle-set-file-modes (filename mode)
  "Set file modes of FILENAME to MODE asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (command (format "chmod %o %s"
                        mode
                        (shell-quote-argument localname))))
    (await (tramp-asyncssh--execute-command vec command))
    nil))

(defun tramp-asyncssh-handle-set-file-times (filename &optional time)
  "Set modification time of FILENAME asynchronously."
  (let* ((vec (tramp-dissect-file-name filename))
         (localname (tramp-file-name-localname vec))
         (time-str (if time
                      (format-time-string "%Y%m%d%H%M.%S" time)
                    ""))
         (command (format "touch %s %s"
                        (if time (concat "-t " time-str) "")
                        (shell-quote-argument localname))))
    (await (tramp-asyncssh--execute-command vec command))
    nil))

;;; Batch Optimizations

(defun tramp-asyncssh-handle-vc-registered (file)
  "Check if FILE is under version control asynchronously.
This uses batch commands to check multiple VCS systems at once."
  (let* ((vec (tramp-dissect-file-name file))
         (localname (tramp-file-name-localname vec))
         (dirname (file-name-directory localname))
         ;; Check for common VCS markers in batch
         (commands (list
                   (format "test -d %s/.git" (shell-quote-argument dirname))
                   (format "test -d %s/.hg" (shell-quote-argument dirname))
                   (format "test -d %s/.svn" (shell-quote-argument dirname))
                   (format "test -d %s/CVS" (shell-quote-argument dirname))))
         (results (await (tramp-asyncssh--batch-execute vec commands))))
    ;; Return t if any VCS directory exists
    (cl-some (lambda (result)
              (= (plist-get result :exit-code) 0))
            results)))


(defconst tramp-asyncssh-file-name-handler-alist
  '((access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-handle-add-name-to-file)
    (copy-file . tramp-asyncssh-handle-copy-file)
    (delete-directory . tramp-asyncssh-handle-delete-directory)
    (delete-file . tramp-asyncssh-handle-delete-file)
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-asyncssh-handle-directory-files)
    (directory-files-and-attributes . tramp-asyncssh-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (expand-file-name . tramp-asyncssh-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-attributes . tramp-asyncssh-handle-file-attributes)
    (file-directory-p . tramp-asyncssh-handle-file-directory-p)
    (file-executable-p . tramp-asyncssh-handle-file-readable-p)
    (file-exists-p . tramp-asyncssh-handle-file-exists-p)
    (file-local-copy . tramp-asyncssh-handle-file-local-copy)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-asyncssh-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-newer-than-file-p . tramp-asyncssh-handle-file-newer-than-file-p)
    (file-readable-p . tramp-asyncssh-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-truename . tramp-handle-file-truename)
    (file-writable-p . tramp-asyncssh-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (insert-directory . tramp-asyncssh-handle-insert-directory)
    (insert-file-contents . tramp-asyncssh-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-asyncssh-handle-make-directory)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (rename-file . tramp-asyncssh-handle-rename-file)
    (set-file-modes . tramp-asyncssh-handle-set-file-modes)
    (set-file-times . tramp-asyncssh-handle-set-file-times)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (vc-registered . tramp-asyncssh-handle-vc-registered)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-asyncssh-handle-write-region))
  "Alist of handler functions for async SSH.")

(defun tramp-asyncssh-file-name-handler (operation &rest args)
  "Invoke the async SSH handler for OPERATION.
First arg specifies the OPERATION, remaining ARGS are passed to the handler."
  (let ((fn (cdr (assq operation tramp-asyncssh-file-name-handler-alist))))
    (if fn
        (save-match-data (apply fn args))
      (tramp-run-real-handler operation args))))

;;;###autoload
(defun tramp-asyncssh-file-name-p (filename)
  "Check if FILENAME is an async SSH Tramp file."
  (and (tramp-tramp-file-p filename)
       (string= (tramp-file-name-method (tramp-dissect-file-name filename))
                tramp-asyncssh-method)))

;; Register the handler
(add-to-list 'tramp-foreign-file-name-handler-alist
             (cons 'tramp-asyncssh-file-name-p
                   'tramp-asyncssh-file-name-handler))

(defun tramp-asyncssh-batch-file-attributes (files)
  "Get attributes for multiple FILES in batch.
FILES should be a list of Tramp file names on the same host.
Returns an alist of (filename . attributes)."
  (when files
    (let* ((vec (tramp-dissect-file-name (car files)))
           (localnames (mapcar (lambda (f)
                                (tramp-file-name-localname
                                 (tramp-dissect-file-name f)))
                              files))
           ;; Build batch stat command
           (commands (mapcar
                     (lambda (localname)
                       (format "stat -c '%s\t%%f\t%%h\t%%u\t%%g\t%%s\t%%Y\t%%X' %s 2>/dev/null || stat -f '%s\t%%p\t%%l\t%%u\t%%g\t%%z\t%%m\t%%a' %s"
                             localname
                             (shell-quote-argument localname)
                             localname
                             (shell-quote-argument localname)))
                     localnames))
           (results (await (tramp-asyncssh--batch-execute vec commands)))
           (alist nil))
      
      (cl-loop for file in files
               for result in results
               for output = (plist-get result :output)
               for parts = (split-string (string-trim output) "\t")
               when (>= (length parts) 8)
               do (let ((attrs (list
                               (not (eq (logand (string-to-number (nth 1 parts) 16) #o40000) 0))
                               (string-to-number (nth 2 parts))
                               (string-to-number (nth 3 parts))
                               (string-to-number (nth 4 parts))
                               nil
                               (seconds-to-time (string-to-number (nth 6 parts)))
                               (seconds-to-time (string-to-number (nth 6 parts)))
                               (string-to-number (nth 5 parts))
                               (format "%o" (string-to-number (nth 1 parts) 16))
                               nil nil nil)))
                    (push (cons file attrs) alist)))
      
      (nreverse alist))))

(defun tramp-asyncssh-batch-file-exists-p (files)
  "Check existence of multiple FILES in batch.
FILES should be a list of Tramp file names on the same host.
Returns an alist of (filename . exists-p)."
  (when files
    (let* ((vec (tramp-dissect-file-name (car files)))
           (localnames (mapcar (lambda (f)
                                (tramp-file-name-localname
                                 (tramp-dissect-file-name f)))
                              files))
           (commands (mapcar
                     (lambda (localname)
                       (format "test -e %s" (shell-quote-argument localname)))
                     localnames))
           (results (await (tramp-asyncssh--batch-execute vec commands)))
           (alist nil))
      
      (cl-loop for file in files
               for result in results
               for exists = (= (plist-get result :exit-code) 0)
               do (push (cons file exists) alist))
      
      (nreverse alist))))

;;; Cache Management

(defvar tramp-asyncssh--attr-cache (make-hash-table :test 'equal)
  "Cache for file attributes to reduce remote calls.")

(defvar tramp-asyncssh--cache-timeout 5
  "Timeout in seconds for cached attributes.")

(defun tramp-asyncssh--cache-key (filename)
  "Generate cache key for FILENAME."
  (format "%s:%f" filename (float-time)))

(defun tramp-asyncssh--get-cached-attrs (filename)
  "Get cached attributes for FILENAME if still valid."
  (let* ((entry (gethash filename tramp-asyncssh--attr-cache))
         (timestamp (car entry))
         (attrs (cdr entry)))
    (when (and timestamp
               (< (- (float-time) timestamp) tramp-asyncssh--cache-timeout))
      attrs)))

(defun tramp-asyncssh--set-cached-attrs (filename attrs)
  "Cache ATTRS for FILENAME."
  (puthash filename (cons (float-time) attrs) tramp-asyncssh--attr-cache))

(defun tramp-asyncssh--invalidate-cache (filename)
  "Invalidate cache for FILENAME."
  (remhash filename tramp-asyncssh--attr-cache))

(defun tramp-asyncssh-clear-cache ()
  "Clear all cached data."
  (interactive)
  (clrhash tramp-asyncssh--attr-cache)
  (message "Async SSH cache cleared"))

;;; Optimized File Attributes with Caching

(defun tramp-asyncssh-handle-file-attributes-cached (filename &optional id-format)
  "Get attributes of FILENAME with caching."
  (or (tramp-asyncssh--get-cached-attrs filename)
      (let ((attrs (tramp-asyncssh-handle-file-attributes filename id-format)))
        (when attrs
          (tramp-asyncssh--set-cached-attrs filename attrs))
        attrs)))

;;; Advanced Batch Operations

(defun tramp-asyncssh-batch-read-files (files)
  "Read multiple FILES in batch and return an alist of (filename . content).
All files must be on the same remote host."
  (when files
    (let* ((vec (tramp-dissect-file-name (car files)))
           (localnames (mapcar (lambda (f)
                                (tramp-file-name-localname
                                 (tramp-dissect-file-name f)))
                              files))
           ;; Use base64 to safely transmit binary content
           (commands (mapcar
                     (lambda (localname)
                       (format "base64 %s 2>/dev/null || echo ERROR"
                             (shell-quote-argument localname)))
                     localnames))
           (results (await (tramp-asyncssh--batch-execute vec commands)))
           (alist nil))
      
      (cl-loop for file in files
               for result in results
               for output = (plist-get result :output)
               for exit-code = (plist-get result :exit-code)
               when (and (= exit-code 0)
                        (not (string-match-p "ERROR" output)))
               do (let ((content (base64-decode-string output)))
                    (push (cons file content) alist)))
      
      (nreverse alist))))

(defun tramp-asyncssh-batch-write-files (file-content-alist)
  "Write multiple files in batch.
FILE-CONTENT-ALIST is an alist of (filename . content).
All files must be on the same remote host."
  (when file-content-alist
    (let* ((vec (tramp-dissect-file-name (caar file-content-alist)))
           (commands (mapcar
                     (lambda (pair)
                       (let* ((filename (car pair))
                              (content (cdr pair))
                              (localname (tramp-file-name-localname
                                        (tramp-dissect-file-name filename)))
                              (encoded (base64-encode-string content t)))
                         (format "echo '%s' | base64 -d > %s"
                               encoded
                               (shell-quote-argument localname))))
                     file-content-alist)))
      (await (tramp-asyncssh--batch-execute vec commands))
      t)))


(defun tramp-asyncssh-cleanup-connections ()
  "Clean up dead connections from the connection pool."
  (interactive)
  (let ((dead-keys nil))
    (maphash (lambda (key conn)
              (let ((proc (plist-get conn :process)))
                (unless (and proc (process-live-p proc))
                  (push key dead-keys)
                  (let ((buf (plist-get conn :buffer)))
                    (when (buffer-live-p buf)
                      (kill-buffer buf))))))
            tramp-asyncssh--connections)
    (dolist (key dead-keys)
      (remhash key tramp-asyncssh--connections))
    (message "Cleaned up %d dead connections" (length dead-keys))))

(defun tramp-asyncssh-close-all-connections ()
  "Close all async SSH connections."
  (interactive)
  (maphash (lambda (key conn)
            (let ((proc (plist-get conn :process))
                  (buf (plist-get conn :buffer)))
              (when (process-live-p proc)
                (delete-process proc))
              (when (buffer-live-p buf)
                (kill-buffer buf))))
          tramp-asyncssh--connections)
  (clrhash tramp-asyncssh--connections)
  (message "All async SSH connections closed"))


(defun tramp-asyncssh-setup ()
  "Set up async SSH method for Tramp."
  (add-to-list 'tramp-methods
               (assoc tramp-asyncssh-method tramp-methods))
  (tramp-register-foreign-file-name-handler
   'tramp-asyncssh-file-name-p
   'tramp-asyncssh-file-name-handler))

;; Auto-setup on load
(with-eval-after-load 'tramp
  (tramp-asyncssh-setup))

;;;
(defvar tramp-asyncssh--completion-cache (make-hash-table :test 'equal)
  "Cache of directory listings.")

(defun tramp-asyncssh-handle-file-name-all-completions-nonblocking (filename directory)
  "Return cached completions immediately, fetch in background."
  (let* ((cached (gethash directory tramp-asyncssh--completion-cache)))
    
    ;; Start background fetch if not cached
    (unless cached
      (let* ((vec (tramp-dissect-file-name directory))
             (localname (tramp-file-name-localname vec)))
        (make-thread
         (lambda ()
           (condition-case nil
               (let* ((command (format "cd %s && ls -1ap" 
                                     (shell-quote-argument localname)))
                      (output (await (tramp-asyncssh--execute-command vec command)))
                      (files (split-string output "\n" t)))
                 (puthash directory 
                         (cl-remove-if (lambda (f) (member f '("." ".."))) files)
                         tramp-asyncssh--completion-cache))
             (error nil))))))
    
    ;; Return cached results immediately (or empty list)
    (let ((results (or cached '())))
      (all-completions filename results))))

;; Replace the handler
(setf (alist-get 'file-name-all-completions tramp-asyncssh-file-name-handler-alist)
      'tramp-asyncssh-handle-file-name-all-completions-nonblocking)

(provide 'tramp-asyncssh)
