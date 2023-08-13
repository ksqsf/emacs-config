;; -*- lexical-binding: t; -*-

(require 'gptel)
(require 'flymake)

(defconst flygpt-prompt
  "You are a professional programming expert, and are asked to find any potential problems in the code, including bugs, security vulnerabilities, style issues, and documentation/comment conformance.

You report your findings in JSON format:
[{ \"line\": <line number>, \"diag\": <detailed problem description> },
 ... ]

If nothing is wrong, reply [].")

(defun line-numbered-string (str)
  (with-temp-buffer
    (setq cnt 0)
    (dolist (line (split-string str "\n"))
      (cl-incf cnt)
      (insert (format "%d: %s\n" cnt line)))
    (buffer-string)))

;;;###autoload
(defun flygpt-buffer ()
  (interactive)
  (let* ((source-buffer (current-buffer))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (gptel-api-key-from-auth-source)))))
         (url-proxy-services '(("https" . "127.0.0.1:7890")))
         (url-request-data (json-encode `(("model" . "gpt-3.5-turbo-16k")
                                          ("temperature" . 0)
                                          ("messages" . [(:role "system" :content ,flygpt-prompt)
                                                         (:role "user" :content ,(line-numbered-string (buffer-string)))])))))
    (url-retrieve
     "https://api.openai.com/v1/chat/completions"
     (lambda (status)
       (goto-char (point-min))
       (search-forward-regexp "^$")
       (let* ((json-object-type 'plist)
              (api-resp (json-read))
              (diags-json (plist-get (plist-get (seq-first (plist-get api-resp :choices)) :message) :content))
              (diags (with-temp-buffer
                       (insert diags-json)
                       (goto-char (point-min))
                       (json-read))))
         (with-current-buffer (get-buffer-create "*flygpt*")
           (erase-buffer)
           (cl-loop
            for diag in (seq-into diags 'list)
            for msg = (plist-get diag :diag)
            for line = (plist-get diag :line)
            do (insert (format "Line %d: %s\n" line msg))
            finally (display-buffer "*flygpt*"))))))))

(setq flymake-gpt-buffer nil)

(defun flymake-gpt (report-fn &rest _)
  (when flymake-gpt-buffer
    (kill-process (get-buffer-process flymake-gpt-buffer))
    (kill-buffer flymake-gpt-buffer))
  (let* ((source-buffer (current-buffer))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (gptel-api-key-from-auth-source)))))
         (url-proxy-services '(("https" . "127.0.0.1:7890")))
         (url-request-data (json-encode `(("model" . "gpt-3.5-turbo-16k")
                                          ("temperature" . 0)
                                          ("messages" . [(:role "system" :content ,flygpt-prompt)
                                                         (:role "user" :content ,(line-numbered-string (buffer-string)))])))))
    (url-retrieve
     "https://api.openai.com/v1/chat/completions"
     (lambda (status)
       (goto-char (point-min))
       (search-forward-regexp "^$")
       (let* ((json-object-type 'plist)
              (api-resp (json-read))
              (diags-json (plist-get (plist-get (seq-first (plist-get api-resp :choices)) :message) :content))
              (diags (with-temp-buffer
                       (insert diags-json)
                       (goto-char (point-min))
                       (json-read))))
         (cl-loop
          for diag in (seq-into diags 'list)
          for msg = (plist-get diag :diag)
          for line = (plist-get diag :line)
          for (beg . end) = (flymake-diag-region source-buffer line)
          collect (flymake-make-diagnostic source-buffer beg end :warning msg)
          into flymake-diags
          finally (funcall report-fn flymake-diags))
         (kill-buffer flymake-gpt-buffer)
         (setq flymake-gpt-buffer nil))))))

;;;###autoload
(defun turn-on-flymake-gpt ()
  (interactive)
  (add-hook 'flymake-diagnostic-functions 'flymake-gpt nil t)
  (setq flymake-no-changes-timeout nil)
  (flymake-mode))

(provide 'flygpt)
