;;; ERC

(use-package erc
  :commands (erc)
  :config
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#emacs" "#haskell")
          ("irc.mozilla.org" "#rust"))))
