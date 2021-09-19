(with-eval-after-load 'telega
  (add-to-list 'telega-proxies '(:server "127.0.0.1" :port 7891 :enable t :type (:@type "proxyTypeSocks5"))))
