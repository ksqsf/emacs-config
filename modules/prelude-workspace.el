;;; prelude-workspace.el ---                         -*- lexical-binding: t; -*-

(use-package bufferlo
  :hook (after-init . bufferlo-mode)
  :config
  (diminish 'bufferlo-mode)
  (with-eval-after-load 'consult
    (defvar my:bufferlo-consult--source-local-buffers
      (list :name "Local Buffers"
            :narrow   ?l
            :category 'buffer
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'bufferlo-local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Local Bufferlo buffer candidate source for `consult-buffer'.")

    (defvar my:bufferlo-consult--source-other-buffers
      (list :name "Other Buffers"
            :narrow   ?b
            :category 'buffer
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :items    (lambda () (consult--buffer-query
                                  :predicate #'bufferlo-non-local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Non-local Bufferlo buffer candidate source for `consult-buffer'.")

    (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-other-buffers)
    (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-local-buffers)))

(use-package tabspaces
  :disabled
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice "*scratch*")
  :bind
  (("C-c TAB TAB" . tabspaces-switch-or-create-workspace))
  :config
  (setq tabspaces-use-filtered-buffers-as-default t
        tabspaces-default-tab "Default"
        tabspaces-remove-to-default t
        tabspaces-include-buffers '("*scratch*"))

  ;; consult integration
  (with-eval-after-load 'consult
    (consult-customize
     consult--source-buffer
     :hidden t
     :default nil
     :state nil
     :narrow ?b)
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(provide 'prelude-workspace)
;;; prelude-workspace.el ends here
