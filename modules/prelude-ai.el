;;; -*- lexical-binding: t; -*-

(defun openai-api-key ()
  (require 'gptel)
  (gptel-api-key-from-auth-source "api.openai.com"))

(defun deepseek-api-key ()
  (require 'gptel)
  (gptel-api-key-from-auth-source "api.deepseek.com"))

(defun anthropic-api-key ()
  (require 'gptel)
  (gptel-api-key-from-auth-source "api.anthropic.com"))

(defun tavily-api-key ()
  (require 'gptel)
  (gptel-api-key-from-auth-source "api.tavily.com"))

(use-package gptel
  :bind (("C-h RET" . gptel-send)  ;; C-u C-h RET for gptel-menu
         ("C-h C-h" . gptel))
  :config
  (setq gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (writing . "You are a large language model and a writing assistant. Respond concisely.")
          (chat . "You are a large language model and a conversation partner. Respond concisely.")
          (bug . "You are a large language model and a careful programmer. The supplied code doesn't work, or contains bugs. Describe each problem using only one sentence. Provide fixes without changing the old behavior.")))

  ;; DeepSeek
  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key #'deepseek-api-key)

  ;; Anthropic
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'anthropic-api-key)

  ;; Default backend and model
  (setopt gptel-model 'claude-3-7-sonnet-20250219
          gptel-backend (cdr (assoc "Claude" gptel--known-backends))
          gptel-default-mode 'markdown-mode)

  (gptel-make-tool
   :name "create_python_repl"
   :function (lambda ()
               (run-python nil t)
               (pop-to-buffer (python-shell-get-buffer)))
   :description "Create a new python repl for this session"
   :args nil
   :category "emacs")

  (gptel-make-tool
   :name "send_python_to_repl"
   :function (lambda (code)
               (python-shell-send-string code))
   :args (list '(:name "code"
                       :type string
                       :description "python code to execute"))
   :description "Send some python code to the python repl for this session and execute it"
   :category "emacs")

  (gptel-make-tool
   :category "web"
   :name "search"
   :function (lambda (keyword)
               (tavily-search keyword))
   :description "Search the Internet"
   :args (list '(:name "keyword"
                       :type string
                       :description "The keyword to search"))))

(use-package dall-e-shell
  :config
  (setq dall-e-shell-openai-key #'gptel-api-key-from-auth-source))

(use-package minuet
  :vc (:fetcher github :repo "milanglacier/minuet-ai.el")
  :bind
  (("M-i" . #'minuet-show-suggestion)
   ("M-I" . #'minuet-complete-with-minibuffer)
   :map minuet-active-mode-map
   ("M-p" . #'minuet-previous-suggestion)
   ("M-n" . #'minuet-next-suggestion)
   ("TAB" . #'minuet-accept-suggestion)
   ("C-n" . #'minuet-accept-suggestion-line)
   ("C-g" . #'minuet-dismiss-suggestion))
  :config

  (defun minuet-use-openai ()
    "Use gpt-4o-mini for code auto-completion."
    (interactive)
    (setq minuet-provider 'openai)
    (plist-put minuet-openai-options :api-key #'gptel-api-key-from-auth-source))

  (defun minuet-use-deepseek ()
    "Use deepseek-chat for code auto-completion."
    (interactive)
    (setq minuet-provider 'openai-fim-compatible)
    (plist-put minuet-openai-fim-compatible-options :endpoint "https://api.deepseek.com/chat/completions")
    (plist-put minuet-openai-fim-compatible-options :api-key #'deepseek-api-key))

  (defun minuet-use-claude ()
    "Use claude-3.5-sonnet for code auto-completion."
    (interactive)
    (setq minuet-provider 'claude)
    (plist-put minuet-claude-options :api-key #'anthropic-api-key))

  (minuet-use-claude))

(use-package chatgpt-shell
  :custom
  (chatgpt-shell-anthropic-key #'anthropic-api-key)
  (chatgpt-shell-openai-key #'openai-api-key)
  (chatgpt-shell-deepseek-key #'deepseek-api-key))

(defun tavily-search (query &optional search-depth max-results)
  "Perform a search using the Tavily API and return results as JSON string.
API-KEY is your Tavily API key.
QUERY is the search query string.
Optional SEARCH-DEPTH is either \"basic\" (default) or \"advanced\".
Optional MAX-RESULTS is the maximum number of results (default 10)."
  (let* ((url "https://api.tavily.com/search")
         (search-depth (or search-depth "basic"))
         (max-results (or max-results 10))
         (request-data
          `(("api_key" . ,(tavily-api-key))
            ("query" . ,query)
            ("search_depth" . ,search-depth)
            ("max_results" . ,max-results))))
    (plz 'post url
         :headers '(("Content-Type" . "application/json"))
         :body (json-encode request-data)
         :as 'string)))

(provide 'prelude-ai)
