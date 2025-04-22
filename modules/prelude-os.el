;;; -*- lexical-binding: t; -*-

;; talk with os-level utilities

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python3 -m json.tool" (buffer-name) t)))

(defun open-directory-here ()
  "Open the current directory in the GUI file manager.

In most cases, this means the current directory of the current buffer."
  (interactive)
  (k|with-suppressed-message
    (shell-command (format "%s %s" k|default-opener (shell-quote-argument (expand-file-name default-directory))))))
(global-set-key (kbd "C-c d") #'open-directory-here)

(defun open-term-here ()
  "Open `default-directory' in iTerm2."
  (interactive)
  (k|with-suppressed-message
    (shell-command (format "open -a iTerm %s" (shell-quote-argument (expand-file-name default-directory))))))
(defalias 'iterm2 'open-term-here)

(when k|mac
  (defun dic ()
    (interactive)
    (shell-command-to-string
     (format "open dict://%s" (word-at-point)))))

;;
;; Recenter the frame on startup.
;; The following code is from https://christiantietze.de/posts/2021/06/emacs-center-window-on-current-monitor/
;;
(defun ct/frame-monitor-usable-height (&optional frame)
  "Return the usable height in pixels of the monitor of FRAME.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame.

Uses the monitor's workarea. See `display-monitor-attributes-list'."
  (cadddr (frame-monitor-workarea frame)))

(defun ct/frame-monitor-usable-width (&optional frame)
  "Return the usable width in pixels of the monitor of FRAME.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame.

Uses the monitor's workarea. See `display-monitor-attributes-list'."
  (caddr (frame-monitor-workarea frame)))

(defun ct/center-box (w h cw ch)
  "Center a box inside another box.

Returns a list of `(TOP LEFT)' representing the centered position
of the box `(w h)' inside the box `(cw ch)'."
  (list (/ (- cw w) 2) (/ (- ch h) 2)))

(defun ct/frame-get-center (frame)
  "Return the center position of FRAME on it's display."
  (ct/center-box (frame-pixel-width frame) (frame-pixel-height frame)
                 (ct/frame-monitor-usable-width frame) (ct/frame-monitor-usable-height frame)))

(defun ct/frame-center (&optional frame)
  "Center a frame on the screen."
  (interactive)
  (let* ((frame (or frame (selected-frame)))
         (center (ct/frame-get-center frame)))
    (apply 'set-frame-position (flatten-list (list frame center)))))

;;; WSL2
;;; Requires 'wslu' (https://wslutiliti.es/wslu/install.html)
(defun wsl-copy (beg end)
  "In a WSL2 environment, copy region to the system clipboard."
  (interactive "r")
  (k|with-suppressed-message
    (let ((default-directory "/"))
      (shell-command-on-region beg end "clip.exe" " *wsl-copy*")))
  (deactivate-mark))

(defun wsl-get-clipboard ()
  "In a WSL2 environment, get the clipboard text."
  (let ((clipboard
         (let ((default-directory "/"))
           (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2>/dev/null")
           ;; WSLu utility
           ;; (shell-command-to-string "wslclip --get")
           )))
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard))
    (setq clipboard (substring clipboard 0 -1))
    clipboard))

(defun wsl-paste ()
  "In a WSL2 environment, paste the text from the system clipboard."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (vterm-insert (wsl-get-clipboard))
    (insert (wsl-get-clipboard))))

(when k|wsl
  (advice-add 'gui-select-text :before
              (lambda (text)
                (when select-enable-clipboard
                  (with-temp-buffer
                    (insert text)
                    (wsl-copy (point-min) (point-max)))))))

(provide 'prelude-os)
