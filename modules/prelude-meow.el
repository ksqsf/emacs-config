;;; -*- lexical-binding: t; -*-
;; This file define my editing environment using Meow.

(add-hook 'vterm-mode-hook #'meow-insert-mode)

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   '("j" . imenu)
   '("k" . kill-buffer)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("p" . projectile-command-map)
   '("f" . find-file)
   '("s" . save-buffer)
   '("r" . k/run)
   '("T" . vterm)
   '("TAB" . switch-to-buffer)
   '("C-s" . switch-to-scratch))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   ;; insert
   '("a" . meow-append)
   '("A" . meow-append-at-end)
   '("i" . meow-insert)
   '("I" . meow-insert-at-begin)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("c" . meow-change)
   ;; move
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("t" . meow-till)
   '("f" . meow-find)
   ;; kill & yank
   '("x" . meow-C-d)
   '("d" . meow-kill)
   '("y" . meow-yank)
   '("Y" . meow-yank-pop)
   ;; undo & redo
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   ;; mark
   '("n" . meow-line)
   '("w" . meow-next-symbol)
   '("b" . meow-back-symbol)
   '("." . meow-symbol)
   '("'" . meow-inner-of-thing)
   '("\"" . meow-bounds-of-thing)
   '("p" . meow-pop-selection)
   ;; other
   '("g" . meow-cancel)
   '("\\" . quoted-insert)
   '("<f3>" . meow-start-kmacro)
   '("<f4>" . meow-end-or-call-kmacro)
   '("<escape>" . meow-last-buffer)))

(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  (meow-setup))

(provide 'prelude-meow)
