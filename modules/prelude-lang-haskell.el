;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode))

;; https://github.com/digital-asset/ghcide
;; https://github.com/haskell/haskell-ide-engine#using-hie-with-emacs
(use-package lsp-haskell
  :disabled
  :hook (haskell-mode . lsp)
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper"))

;; Intero is discontinued (Nov 2019)
(use-package intero
  :hook (haskell-mode . intero-mode)
  :config
  ;; See https://github.com/chrisdone/intero/pull/643
  (defun intero-get-info-of (thing)
    "Get info for THING."
    (let ((optimistic-result
           (replace-regexp-in-string
            "\n$" ""
            (intero-blocking-call
             'backend
             (format ":info %s" thing)))))
      (if (string-match-p "^<interactive>" optimistic-result)
          ;; Load the module Interpreted so that we get information,
          ;; then restore bytecode.
          (progn (intero-async-call
                  'backend
                  ":set -fbyte-code")
                 (set-buffer-modified-p t)
                 (save-buffer)
                 (unless (member 'save flycheck-check-syntax-automatically)
                   (intero-async-call
                    'backend
                    (concat ":load " (intero-path-for-ghci (intero-temp-file-name)))))
                 (mapc
                  (lambda (flag)
                    (intero-async-call
                     'backend
                     (concat ":set " flag)))
                  (intero-ghci-output-flags))
                 (replace-regexp-in-string
                  "\n$" ""
                  (intero-blocking-call
                   'backend
                   (format ":info %s" thing))))
        optimistic-result)))

  (defun intero-start-process-in-buffer (buffer &optional targets source-buffer stack-yaml)
    "Start an Intero worker in BUFFER.
Uses the specified TARGETS if supplied.
Automatically performs initial actions in SOURCE-BUFFER, if specified.
Uses the default stack config file, or STACK-YAML file if given."
    (if (buffer-local-value 'intero-give-up buffer)
        buffer
      (let* ((process-info (intero-start-piped-process buffer targets stack-yaml))
             (arguments (plist-get process-info :arguments))
             (options (plist-get process-info :options))
             (process (plist-get process-info :process)))
        (set-process-query-on-exit-flag process nil)
        (mapc
         (lambda (flag)
           (process-send-string process (concat ":set " flag "\n")))
         (intero-ghci-output-flags))
        (process-send-string process ":set -fdefer-type-errors\n")
        (process-send-string process ":set -fdiagnostics-color=never\n")
        (process-send-string process ":set prompt \"\\4\"\n")
        (with-current-buffer buffer
          (erase-buffer)
          (when stack-yaml
            (setq intero-stack-yaml stack-yaml))
          (setq intero-targets targets)
          (setq intero-start-time (current-time))
          (setq intero-source-buffer source-buffer)
          (setq intero-arguments arguments)
          (setq intero-starting t)
          (setq intero-callbacks
                (list (list (cons source-buffer
                                  buffer)
                            (lambda (buffers msg)
                              (let ((source-buffer (car buffers))
                                    (process-buffer (cdr buffers)))
                                (with-current-buffer process-buffer
                                  (when (string-match "^Intero-Service-Port: \\([0-9]+\\)\n" msg)
                                    (setq intero-service-port (string-to-number (match-string 1 msg))))
                                  (setq-local intero-starting nil))
                                (when source-buffer
                                  (with-current-buffer source-buffer
                                    (when flycheck-mode
                                      (run-with-timer 0 nil
                                                      'intero-call-in-buffer
                                                      (current-buffer)
                                                      'intero-flycheck-buffer)))))
                              (message "Booted up intero!"))))))
        (set-process-filter
         process
         (lambda (process string)
           (when intero-debug
             (message "[Intero] <- %s" string))
           (when (buffer-live-p (process-buffer process))
             (with-current-buffer (process-buffer process)
               (goto-char (point-max))
               (insert string)
               (when (and intero-try-with-build
                          intero-starting)
                 (let ((last-line (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
                   (if (string-match-p "^Progress" last-line)
                       (message "Booting up intero (building dependencies: %s)"
                                (downcase
                                 (or (car (split-string (replace-regexp-in-string
                                                         "\u0008+" "\n"
                                                         last-line)
                                                        "\n" t))
                                     "...")))
                     (message "Booting up intero ..."))))
               (intero-read-buffer)))))
        (set-process-sentinel process 'intero-sentinel)
        buffer)))

  (define-advice intero-ghci-output-flags (:override ())
    (with-current-buffer (intero-buffer 'backend)
      (let ((current-version (mapcar #'string-to-number (split-string (intero-ghc-version) "\\."))))
        (if (intero-version>= current-version '(8 4 1))
            '("-fno-code" "-fwrite-interface")
          '("-fobject-code"))))))

(provide 'prelude-lang-haskell)
