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
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key #'deepseek-api-key
    :models '(deepseek-chat deepseek-reasoner))

  ;; Anthropic
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'anthropic-api-key)

  ;; Default backend and model
  (push '(claude-3-7-sonnet-20250219
          :description "Highest level of intelligence and capability"
          :capabilities (media tool-use cache)
          :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
          :context-window 200
          :input-cost 3
          :output-cost 15
          :cutoff-date "2024-04")
        gptel--anthropic-models)
  (setopt gptel-model 'claude-3-7-sonnet-20250219
          gptel-backend (cdr (assoc "Claude" gptel--known-backends))
          gptel-default-mode 'markdown-mode))

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


(provide 'prelude-ai)
