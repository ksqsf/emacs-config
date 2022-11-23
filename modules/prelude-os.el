;;; -*- lexical-binding: t; -*-

;; talk with os-level utilities

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python3 -m json.tool" (buffer-name) t)))

(defun open-directory-here ()
  "Open `default-directory'  with `system-opener'.

In most cases, this means the current directory of the current buffer."
  (interactive)
  (k|with-suppressed-message
    (shell-command (format "%s %s" k|default-opener (shell-quote-argument (expand-file-name default-directory))))))

(global-set-key (kbd "C-c d") #'open-directory-here)


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
  (let* ((frame (or (and (boundp 'frame) frame) (selected-frame)))
         (center (ct/frame-get-center frame)))
    (apply 'set-frame-position (flatten-list (list frame center)))))

(provide 'prelude-os)
