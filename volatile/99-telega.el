(with-eval-after-load 'telega
  (add-to-list 'telega-proxies '(:server "127.0.0.1" :port 7890 :enable t :type (:@type "proxyTypeHttp"))))
