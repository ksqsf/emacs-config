;;; logseq.el --- Interface to Logseq remote API     -*- lexical-binding: t; -*-

;; Author: ksqsf <i@ksqsf.moe>
;; URL: https://github.com/ksqsf/emacs-config
;; Version: 0.1
;; Package-Requires: ((emacs "27") request)

;; Copyright (C) 2023  ksqsf

;;; Commentary:

;; This module provides (1) a complete wrapper to Logseq remote API
;; and (2) commands to quickly issue queries.

;; For each API, there will be two functions.
;;   1. (apiName  arg1 arg2 ...)
;;   2. (apiName. logseq-instance arg1 arg2 ...)
;; For example:
;;   1. (logseq.DB.q query-str)
;;   2. (logseq.DB.q. logseq-instance query-str)
;;
;; logseq-instance can be constructed with 'make-logseq'.
;;
;; The first variant uses the default instance constructed from user
;; options in the customization group 'logseq'.

;; 'logseq-query-mode' provides a quick way to query logseq graphs
;; inside Emacs.  It is very useful for writing complex queries.
;;
;; 1. In any buffer, say *scratch*, activate 'logseq-query-mode'.
;; 2. Write your queries, either advanced or simple.
;;    You can use 'C-c C-i' to insert a template.
;; 3. 'C-c C-c' (or 'logseq-run-query-dwim') to issue your query.
;;    The results will be nicely formatted and dispalyed in
;;    *logseq-query*.

;;; Code:

(require 'json)
(require 'request)

(defgroup logseq nil
  "Customization group for logseq.el.")

(defcustom logseq-default-host "127.0.0.1"
  "The default host used when no `logseq' instance is provided."
  :group 'logseq
  :type 'string)

(defcustom logseq-default-port 12315
  "The default port used when no `logseq' instance is provided."
  :group 'logseq
  :type 'integer)

(defcustom logseq-default-token ""
  "The default token used when no `logseq' instance is provided."
  :group 'logseq
  :type 'string)

(cl-defstruct (logseq (:type vector))
  "A Logseq instance holds information for future API calls."
  (host "127.0.0.1")
  (port 12315)
  token)

(defvar logseq--default nil
  "The default logseq instance.  Should only by set by
`with-logseq'.")

(defun logseq-default ()
  (or logseq--default
      (make-logseq :host logseq-default-host
                   :port logseq-default-port
                   :token logseq-default-token)))

(defmacro with-logseq (logseq &rest forms)
  "Temporarily set the default instance to LOGSEQ."
  (declare (indent 1))
  `(let ((logseq--default logseq))
     ,@forms))

(cl-defun logseq--call (method args &key logseq)
  "Initiate HTTP request for METHOD with arguments ARGS."
  (setq logseq (or logseq (logseq-default)))
  (let* ((resp (request (format "http://%s:%d/api" (logseq-host logseq) (logseq-port logseq))
                 :sync t
                 :type "POST"
                 :headers `(("Content-Type" . "application/json")
                            ("Authorization" . ,(format "Bearer %s" (logseq-token logseq))))
                 :data (json-encode `(("method" . ,method) ("args" . ,(seq-into args 'vector))))
                 :complete 'ignore))
         (data (request-response-data resp)))
    (if (string-prefix-p "application/json" (request-response-header resp "Content-Type"))
        (json-parse-string data)
      data)))

(defconst logseq--apis '(logseq.settings
                         logseq.updateSettings
                         logseq.once
                         logseq.toggleMainUI
                         logseq.listeners
                         logseq.ready
                         logseq.connected
                         logseq.removeListener
                         logseq.showMainUI
                         logseq.resolveResourceFullUrl
                         logseq.provideStyle
                         logseq.caller
                         logseq.addListener
                         logseq.hideSettingsUI
                         logseq.provideUI
                         logseq.setMainUIInlineStyle
                         logseq.emit
                         logseq.showSettingsUI
                         logseq.listenerCount
                         logseq.removeAllListeners
                         logseq.onSettingsChanged
                         logseq.provideTheme
                         logseq.Experiments
                         logseq.eventNames
                         logseq.FileStorage
                         logseq.provideModel
                         logseq.baseInfo
                         logseq.setMainUIAttrs
                         logseq.useSettingsSchema
                         logseq.hideMainUI
                         logseq.isMainUIVisible
                         logseq.beforeunload
                         logseq.UI.showMsg
                         logseq.UI.closeMsg
                         logseq.App.registerPageMenuItem
                         logseq.App.getUserInfo
                         logseq.App.setRightSidebarVisible
                         logseq.App.showMsg
                         logseq.App.quit
                         logseq.App.registerUIItem
                         logseq.App.setFullScreen
                         logseq.App.onMacroRendererSlotted
                         logseq.App.getInfo
                         logseq.App.onPageHeadActionsSlotted
                         logseq.App.onCurrentGraphChanged
                         logseq.App.registerCommandShortcut
                         logseq.App.getStateFromStore
                         logseq.App.onSidebarVisibleChanged
                         logseq.App.registerCommand
                         logseq.App.setLeftSidebarVisible
                         logseq.App.replaceState
                         logseq.App.setZoomFactor
                         logseq.App.execGitCommand
                         logseq.App.invokeExternalCommand
                         logseq.App.queryElementById
                         logseq.App.onThemeModeChanged
                         logseq.App.openExternalLink
                         logseq.App.pushState
                         logseq.App.getCurrentGraph
                         logseq.App.onRouteChanged
                         logseq.App.queryElementRect
                         logseq.App.registerCommandPalette
                         logseq.App.relaunch
                         logseq.App.getUserConfigs
                         logseq.App.onBlockRendererSlotted
                         logseq.DB.datascriptQuery
                         logseq.DB.onChanged
                         logseq.DB.q
                         logseq.DB.onBlockChanged
                         logseq.Assets.listFilesOfCurrentGraph
                         logseq.Editor.insertBatchBlock
                         logseq.Editor.getAllPages
                         logseq.Editor.createPage
                         logseq.Editor.getBlockProperty
                         logseq.Editor.getBlockProperties
                         logseq.Editor.insertAtEditingCursor
                         logseq.Editor.getCurrentPage
                         logseq.Editor.appendBlockInPage
                         logseq.Editor.getSelectedBlocks
                         logseq.Editor.insertBlock
                         logseq.Editor.getPagesTreeFromNamespace
                         logseq.Editor.onInputSelectionEnd
                         logseq.Editor.scrollToBlockInPage
                         logseq.Editor.moveBlock
                         logseq.Editor.getPreviousSiblingBlock
                         logseq.Editor.exitEditingMode
                         logseq.Editor.getPagesFromNamespace
                         logseq.Editor.getNextSiblingBlock
                         logseq.Editor.getPage
                         logseq.Editor.renamePage
                         logseq.Editor.prependBlockInPage
                         logseq.Editor.deletePage
                         logseq.Editor.editBlock
                         logseq.Editor.checkEditing
                         logseq.Editor.getCurrentPageBlocksTree
                         logseq.Editor.getCurrentBlock
                         logseq.Editor.upsertBlockProperty
                         logseq.Editor.registerSlashCommand
                         logseq.Editor.getPageBlocksTree
                         logseq.Editor.getPageLinkedReferences
                         logseq.Editor.updateBlock
                         logseq.Editor.registerBlockContextMenuItem
                         logseq.Editor.removeBlock
                         logseq.Editor.restoreEditingCursor
                         logseq.Editor.removeBlockProperty
                         logseq.Editor.getBlock
                         logseq.Editor.openInRightSidebar
                         logseq.Editor.setBlockCollapsed
                         logseq.Editor.getEditingBlockContent
                         logseq.Editor.getEditingCursorPosition
                         logseq.Git.saveIgnoreFile
                         logseq.Git.loadIgnoreFile
                         logseq.Git.execCommand)
  "List of symbols of Logseq plugin APIs.

Each symbol corresponds to two Lisp functions.
- logseq.Editor.getBlock uses the default `logseq' instance.
- logseq.Editor.getBlock. has an additional argument to use a custom `logseq' instance.")

(dolist (api logseq--apis)
  (defalias api
    `(lambda (&rest args)
       ,(format "Call Logseq API %s using the default Logseq instance." (symbol-name api))
       (logseq--call ,(symbol-name api) args)))
  (defalias (intern (format "%s." (symbol-name api)))
    `(lambda (logseq &rest args)
       ,(format "Call Logseq API %s using the supplied Logseq instance LOGSEQ." (symbol-name api))
       (logseq--call ,(symbol-name api) args :logseq logseq))))

;;;###autoload
(define-minor-mode logseq-query-mode
  "Edit and run Logseq queries."
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "C-c C-c") 'logseq-run-query-dwim)
            (define-key keymap (kbd "C-c C-i") 'logseq-advanced-query-insert-skeleton)
            keymap))

(defun logseq-run-simple-query ()
  "Run the query in this buffer and display the results in *logseq-query*."
  (interactive)
  (let ((query (buffer-string)))
    (display-buffer (get-buffer-create "*logseq-query*"))
    (with-current-buffer "*logseq-query*"
      (erase-buffer)
      (insert (json-serialize (logseq.DB.q query)))
      (json-pretty-print-buffer-ordered))))

(defun logseq--collect-sexps (&optional start count)
  "Collect COUNT sexps (string reps) as a list.

If COUNT is nil, read until end of buffer."
  (save-excursion
    (let (last)
      (setq last (or start (point)))
      (goto-char last)
      (cl-loop while (or (null count) (> count 0))
               do (forward-sexp)
               until (= last (point))
               for str = (string-trim (buffer-substring last (point)))
               until (string= "" str)
               collect str
               do (setq last (point))
               do (when count (cl-decf count))))))

(defun logseq-run-advanced-query ()
  "Run the query in this buffer and display the results in *logseq-query*.

The first sexp should be the query.  Any sexp following it will
be considered an input."
  (interactive)
  (save-excursion
    (let* ((query-end (scan-sexps (point-min) 1))
           (query (buffer-substring (point-min) query-end))
           (inputs (logseq--collect-sexps query-end)))
      (display-buffer (get-buffer-create "*logseq-query*"))
      (with-current-buffer "*logseq-query*"
        (erase-buffer)
        (insert (json-serialize (apply 'logseq.DB.datascriptQuery query inputs)))
        (json-pretty-print-buffer-ordered)))))

;;;###autoload
(defun logseq-run-query-dwim ()
  "Run the query in this buffer and display the results in *logseq-query*."
  (interactive)
  ;; look at the first non-whitespace character
  ;; if it's (, it is a simple query, otherwise an advanced query.
  (if (save-excursion
        (skip-chars-forward " \t\n\r")
        (looking-at-p "("))
      (logseq-run-simple-query)
    (logseq-run-advanced-query)))

(defconst logseq-advanced-query-skeleton "[:find (pull ?block [*])
 :where
 [?block :block/name ?pagename]]")

;;;###autoload
(defun logseq-advanced-query-insert-skeleton ()
  "Insert a skeleton of the advanced query."
  (interactive)
  (insert logseq-advanced-query-skeleton))

(provide 'logseq)
;;; logseq.el ends here
