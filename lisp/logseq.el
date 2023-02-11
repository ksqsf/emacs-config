(require 'json)
(require 'request)

(defgroup logseq nil
  "Customization group for logseq.el.")

(defcustom logseq-default-host "127.0.0.1"
  "The default host used when no `logseq' instance is provided."
  :group 'logseq
  :type 'string
  :set 'logseq--update-default-instance)

(defcustom logseq-default-port 12315
  "The default port used when no `logseq' instance is provided."
  :group 'logseq
  :type 'integer
  :set 'logseq--update-default-instance)

(defcustom logseq-default-token ""
  "The default token used when no `logseq' instance is provided."
  :group 'logseq
  :type 'string
  :set 'logseq--update-default-instance)

(cl-defstruct (logseq (:type vector))
  "A Logseq instance holds information for future API calls."
  (host "127.0.0.1")
  (port 12315)
  token)

(defvar logseq--default (make-logseq :host logseq-default-host
                                     :port logseq-default-port
                                     :token logseq-default-token))

(defun logseq--update-default-instance (symbol value)
  "Update `logseq--default' after the user options are changed."
  (setq logseq--default (make-logseq :host logseq-default-host
                                     :port logseq-default-port
                                     :token logseq-default-token))
  (set-default-toplevel-value symbol value))

(cl-defun logseq--call (method args &key logseq)
  "Initiate HTTP request for METHOD with arguments ARGS."
  (setq logseq (or logseq logseq--default))
  (let* ((resp (request (format "http://%s:%d/api" (logseq-host logseq) (logseq-port logseq))
                 :sync t
                 :type "POST"
                 :headers `(("Content-Type" . "application/json")
                            ("Authorization" . ,(format "Bearer %s" (logseq-token logseq))))
                 :data (json-encode `(("method" . ,method) ("args" . ,(seq-into args 'vector))))
                 :complete 'ignore))
         (data (request-response-data resp)))
    (condition-case nil
        (json-parse-string  data)
      (json-error data))))

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
       ,(format "Call Logseq API %s using the default Logseq instance LOGSEQ." (symbol-name api))
       (logseq--call ,(symbol-name api) args)))
  (defalias (intern (format "%s." (symbol-name api)))
    `(lambda (logseq &rest args)
       ,(format "Call Logseq API %s using the supplied Logseq instance LOGSEQ." (symbol-name api))
       (logseq--call ,(symbol-name api) args :logseq logseq))))

(define-minor-mode logseq-query-mode
  "Edit and run Logseq queries."
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "C-c C-c") 'logseq-run-query-dwim)
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

(defun logseq-run-advanced-query ()
  "Run the query in this buffer and display the results in *logseq-query*."
  (interactive)
  (let ((query (buffer-string)))
    (display-buffer (get-buffer-create "*logseq-query*"))
    (with-current-buffer "*logseq-query*"
      (erase-buffer)
      (insert (json-serialize (logseq.DB.datascriptQuery query)))
      (json-pretty-print-buffer-ordered))))

(defun logseq-run-query-dwim ()
  "Run the query in this buffer and display the results in *logseq-query*."
  (interactive)
  ;; look at the first non-whitespace character
  ;; if it's (, it is a simple query, otherwise an advanced query.
  (save-excursion
    (skip-chars-forward " \t\n\r")
    (if (looking-at-p "(")
        (logseq-run-simple-query)
      (logseq-run-advanced-query))))

(defconst logseq-advanced-query-skeleton "[:find (pull ?block [*])
 :where
 [?block :block/name ?pagename]]")

(defun logseq-advanced-query-insert-skeleton ()
  "Insert a skeleton of the advanced query."
  (interactive)
  (insert logseq-advanced-query-skeleton))

(provide 'logseq)
