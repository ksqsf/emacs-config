;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "treefold" "treefold.el" (24246 54609 910049
;;;;;;  611000))
;;; Generated autoloads from treefold.el

(autoload 'treefold-toggle "treefold" "\
Toggle whether the current subtree is folded." t nil)

(autoload 'treefold-unfold-all "treefold" "\
Unfold all subtrees in this buffer." t nil)

(autoload 'treefold-forward-subtree "treefold" "\
Go to the position of the indicator for the next N-th subtree.

If N is negative, search backwards.

\(fn &optional N)" t nil)

(autoload 'treefold-backward-subtree "treefold" "\
Go to the position of the indicator for the previous N-th subtree.

If N is negative, search forward.

\(fn &optional N)" t nil)

(autoload 'treefold-mode "treefold" "\
Enable treefold functions in this buffer.

If called interactively, enable Treefold mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treefold" '("treefold--")))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
