;;; -*- lexical-binding: t; -*-

;; See also occur (built-in), embark, consult-line, etc.
(use-package wgrep
  :defer t
  :bind
  (:map grep-mode-map
        ;; occur-style keybinding
        ("e" . wgrep-change-to-wgrep-mode)
        ;; dired-style keybinding
        ("C-x C-q" . wgrep-change-to-wgrep-mode)))

(provide 'prelude-search)
