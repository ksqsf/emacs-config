;;; -*- lexical-binding: t; -*-

;; Prefer regular expression-based searches
(global-set-key [remap isearch-forward] #'isearch-forward-regexp)
(global-set-key [remap query-replace] #'query-replace-regexp)

;; See also occur (built-in), embark, consult-line, etc.
(use-package wgrep
  :defer t
  :bind
  (:map grep-mode-map
        ;; occur-style keybinding
        ("e" . wgrep-change-to-wgrep-mode)
        ;; dired-style keybinding
        ("C-x C-q" . wgrep-change-to-wgrep-mode)))

(defun consult-ripgrep-current-dir ()
  "Search with rg for files in the current directory with INITIAL input."
  (interactive)
  (consult-ripgrep default-directory nil))

(keymap-global-set "M-s M-s" #'consult-ripgrep-current-dir)

(with-eval-after-load 'grep
  (push ".tags" grep-find-ignored-files)
  (push ".git" grep-find-ignored-files))

(with-eval-after-load 'vterm
  (keymap-set vterm-mode-map "M-s" search-map))
(with-eval-after-load 'eat
  (keymap-set eat-semi-char-mode-map "M-s" search-map))

(use-package ast-grep
  :commands (ast-grep-search ast-grep-project ast-grep-directory))

(provide 'prelude-search)
