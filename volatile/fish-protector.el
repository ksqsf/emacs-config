(with-eval-after-load 'telega
  (require 'fish-protector)

  (setq fish-protector-alist
        '((:telega . (or (mode telega-root-mode)
                         (mode telega-chat-mode)))))

  (fish-protector-add-reach-limit
   :telega
   (* 30 60)  ;; 30 minutes
   (lambda (cur-sec)
     (if (> cur-sec (* 60 60))  ;; hard limit: 60 minutes
         (progn
           (kill-buffer "*Telega Root*")
           (fish-protector-alert "Hard limit of Telega reached!"))
       (when (= 0 (% cur-sec (* 5 60)))
         (fish-protector-alert "Soft limit of Telega reached! Please go away from Telega!!")))))

  (fish-protector-start))
