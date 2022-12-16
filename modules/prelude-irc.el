;;; ERC

(use-package rcirc
  :ensure nil
  :defer t
  :custom
  (rcirc-server-alist '(("irc.libera.chat"
                         :channels ("#emacs" "#haskell"))))
  (rcirc-default-nick "ksqsf")
  (rcirc-default-user-name "ksqsf")
  (rcirc-default-full-name "Curious Minds Want to Know"))

(use-package erc
  :ensure nil
  :defer t
  :custom
  (erc-nick "ksqsf")
  (erc-server "irc.libera.chat")
  (erc-autojoin-channels-alist '(("libera.chat" "#haskell" "#emacs")))
  (erc-sasl-user "ksqsf")
  (erc-sasl-auth-source-function 'erc-sasl-auth-source-password-as-host)
  :config
  (add-to-list 'erc-modules 'sasl)
  (defalias 'erc 'erc-tls))

(provide 'prelude-irc)
