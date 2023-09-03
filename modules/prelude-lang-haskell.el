;;; -*- lexical-binding: t; -*-

;; Currently, only haskell-mode is supported.

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :bind (("C-c C-l" . haskell-process-load-or-reload)
         ("C-c C-f" . haskell-mode-format-imports)
         ("<f8>" . haskell-navigate-imports))
  :hook (haskell-mode . interactive-haskell-mode)
  ;; :hook (haskell-mode . k|setup-haskell-prettify-symbols)
  :hook (inferior-haskell-mode . (lambda () (comint-use-persistent-input-history "~/.ghc/ghci_history")))
  :hook (haskell-mode . k|lsp-ensure)
  :hook (after-save . k|hpack-after-save)

  :config

  (defun k|hpack-after-save ()
    (when (string= (buffer-name) "package.yaml")
      (start-process "hpack" " *hpack*" "hpack")))

  (defun k|setup-haskell-prettify-symbols ()
    (setq prettify-symbols-alist
          '(("\\" . ?λ)
            ("forall" . ?∀)
            ("\\/" . ?∨)
            ("/\\" . ?∧)
            (">-" . ?⤚)
            ("-<" . ?⤙)
            (">>-" . ?⤜)
            ("-<<" . ?⤛)
            ("[|" . 10214) ;; ⟦
            ("|]" . 10215) ;; ⟧
            ))
    (prettify-symbols-mode))

  ;; (setq haskell-process-wrapper-function
  ;;       (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))
  )

(use-package speedbar
  :ensure nil
  :after haskell-mode
  :config
  (speedbar-add-supported-extension ".hs"))

(use-package align
  :ensure nil
  :after haskell-mode
  :config
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

(use-package flycheck-haskell
  :after haskell-mode
  :hook (flycheck-mode-hook . flycheck-haskell-setup))

;; (use-package w3m-haddock
;;   :disabled
;;   :ensure w3m
;;   :after haskell-mode
;;   :config
;;   (setq w3m-mode-map (make-sparse-keymap))

;;   (define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url)
;;   (define-key w3m-mode-map (kbd "q") 'bury-buffer)
;;   (define-key w3m-mode-map (kbd "<mouse-1>") 'w3m-maybe-url)
;;   (define-key w3m-mode-map [f5] 'w3m-reload-this-page)
;;   (define-key w3m-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
;;   (define-key w3m-mode-map (kbd "M-<left>") 'w3m-view-previous-page)
;;   (define-key w3m-mode-map (kbd "M-<right>") 'w3m-view-next-page)
;;   (define-key w3m-mode-map (kbd "M-.") 'w3m-haddock-find-tag)

;;   (defun w3m-maybe-url ()
;;     (interactive)
;;     (if (or (equal '(w3m-anchor) (get-text-property (point) 'face))
;;             (equal '(w3m-arrived-anchor) (get-text-property (point) 'face)))
;;         (w3m-view-this-url)))
  
;;   (add-hook 'w3m-display-hook 'w3m-haddock-display))

(use-package ormolu
  :after (haskell-mode)
  ;; :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
        ("C-c C-f" . ormolu-format-buffer)))

(defvar k|latest-stackage-lts nil
  "Cached value of stackage LTS snapshot version.")
(defvar k|stack-proc nil)
(defun k|get-latest-stackage-lts ()
  "Returns the latest version of the Stackage LTS resolver.

This data is fetched from `stack ls snapshots --resolver remote'.
If no data can be fetched, a default value (lts-14.20) is returned."
  (if k|latest-stackage-lts
      k|latest-stackage-lts
    (let ((default "lts-18.25")
          answer)
      (with-temp-buffer
        (message "Fetching version from stack...")
        (call-process "stack" nil (current-buffer) nil
                      "--no-terminal" "ls" "snapshots" "--lts" "remote")
        (goto-char (point-max))
        (if (re-search-backward "Resolver name: \\(lts-[0-9]+\\.[0-9]+\\)" nil t)
            (setq answer (match-string-no-properties 1))
          (warn "Unable to get the version number! Returning a default value: %s" default)
          (setq answer default))
        (setq k|latest-stackage-lts answer)))))

(define-derived-mode haskell-iface-mode fundamental-mode "Haskell Iface"
  "View the contents of Haskell interface files, by invoking `ghc --show-iface'.

This mode is not reliable: the ghc version will probably not
match that of the file."
  (delete-region (point-min) (point-max))
  (call-process "ghc" nil t nil "--show-iface" (buffer-file-name))
  (read-only-mode)
  (set-buffer-modified-p nil)
  (goto-char (point-min)))

(add-to-list 'auto-mode-alist '("\\.hi\\'" . haskell-iface-mode))

(defun haskell ()
  (interactive)
  (find-file "/tmp/Main.hs")
  (when (= 0 (- (point-max) (point-min)))
    (insert "module Main where\n\nmain = undefined\n\n")))

(defun cabal (package-name)
  "Create a new cabal package under /tmp."
  (interactive "sPackage name: ")
  (let ((old-pwd default-directory))
    (cd "/tmp")
    (mkdir package-name t)
    (cd (concat "/tmp/" package-name))
    (with-temp-buffer
      (insert "cabal-version: 2.4\n")
      (insert (concat "name: " package-name "\n"))
      (insert "version: 0.1.0.0
library
  default-language: Haskell2010
  exposed-modules:
    Lib
  ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints\n")
      (insert "  default-extensions: NoImplicitPrelude OverloadedStrings\n")
      (insert "  build-depends: base, text, bytestring, array, vector, stm, async, directory, filepath, rio, unliftio, microlens-platform\n")
      (write-file (concat package-name ".cabal")))
    (async-shell-command "git init")
    (cd old-pwd))
  (find-file (concat "/tmp/" package-name "/Lib.hs"))
  (delete-region (point-min) (point-max))
  (insert "module Lib where

import RIO

run :: IO ()
run = runSimpleApp $ do
  pure ()"))

(provide 'prelude-lang-haskell)
