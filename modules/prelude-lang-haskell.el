;;; -*- lexical-binding: t; -*-

;; Currently, only haskell-mode is supported.

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :bind (("C-c C-l" . haskell-process-load-or-reload)
         ("C-c C-f" . haskell-mode-format-imports)
         ("<f8>" . haskell-navigate-imports))
  :hook (haskell-mode . yas-minor-mode)
  :hook (haskell-mode . interactive-haskell-mode)
  ;; :hook (haskell-mode . flyspell-prog-mode)
  :config
  (defun hpack-after-save-hook ()
    (when (string= (buffer-name) "package.yaml")
      (start-process "hpack" " *hpack*" "hpack")))
  (add-hook 'after-save-hook #'hpack-after-save-hook)

  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode)))))

  (setq haskell-process-wrapper-function
        (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

  (with-eval-after-load 'speedbar
      (speedbar-add-supported-extension ".hs")))

(use-package w3m-haddock
  :ensure w3m
  :after haskell-mode
  :config
  (setq w3m-mode-map (make-sparse-keymap))

  (define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url)
  (define-key w3m-mode-map (kbd "q") 'bury-buffer)
  (define-key w3m-mode-map (kbd "<mouse-1>") 'w3m-maybe-url)
  (define-key w3m-mode-map [f5] 'w3m-reload-this-page)
  (define-key w3m-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
  (define-key w3m-mode-map (kbd "M-<left>") 'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "M-<right>") 'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "M-.") 'w3m-haddock-find-tag)

  (defun w3m-maybe-url ()
    (interactive)
    (if (or (equal '(w3m-anchor) (get-text-property (point) 'face))
            (equal '(w3m-arrived-anchor) (get-text-property (point) 'face)))
        (w3m-view-this-url)))
  
  (add-hook 'w3m-display-hook 'w3m-haddock-display))

(defun pointfree (expr)
  "Get the point-free version of EXPR from http://pointfree.io/."
  (interactive))

;; https://github.com/digital-asset/ghcide
;; https://github.com/haskell/haskell-ide-engine#using-hie-with-emacs
(use-package lsp-haskell
  :disabled
  :hook (haskell-mode . lsp)
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper"))

(use-package dante
  :disabled ;; not very useful. haskell-interactive-mode is good enough.
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flymake-mode)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-hook 'haskell-mode-hook 'dante-mode))

(defvar *prelude--latest-stackage-lts-version* nil
  "Cached value for stackage lts version.")
(defvar *prelude--stack-proc* nil)

(defun prelude--get-latest-stackage-lts-version ()
  "Returns the latest version of the Stackage LTS resolver.

This data is fetched from 'stack ls snapshots --resolver remote'.
If no data can be fetched, a default value (lts-14.20) is returned."
  (if *prelude--latest-stackage-lts-version*
      *prelude--latest-stackage-lts-version*
    (let ((default "lts-14.20") answer)
      (with-temp-buffer
        (message "Fetching version from stack...")
        (call-process "stack" nil (current-buffer) nil
                      "--no-terminal" "ls" "snapshots" "--lts" "remote")
        (goto-char (point-max))
        (if (re-search-backward "Resolver name: \\(lts-[0-9]+\\.[0-9]+\\)" nil t)
            (setq answer (match-string-no-properties 1))
          (warn "Unable to get the version number! Returning a default value: %s" default)
          (setq answer default))
        (setq *prelude--latest-stackage-lts-version* answer)))))

(provide 'prelude-lang-haskell)
