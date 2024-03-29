(use-package nix-mode
  :mode (".nix\\'" . nix-mode))

(defun nix-shell-vterm ()
  "Start nix-shell here."
  (interactive)
  (let ((vterm (vterm "*nix-shell*")))
    (with-current-buffer vterm
      (vterm-send-string "nix-shell")
      (vterm-send-return))))

(defvar k|nix t
  "Indicate that the system is managed by Nix, so that we can leverage Nix power. 

If it's enabled, some programming environments may benefit from nix-shell.")

(use-package nix-sandbox
  :commands (nix-shell-command nix-shell nix-compile nix-find-sandbox nix-current-sandbox nix-executable-find))

(defun k|nix-refresh ()
  (interactive)
  (save-buffer)
  (get-buffer-create "*nix*")
  (with-current-buffer "*nix*"
    (delete-region (point-min) (point-max))
    (start-process "nix" "*nix*" "nix-env" "-iA" "nixpkgs.myMac")
    (display-buffer "*nix*")
    (goto-char (point-min))))

(provide 'prelude-nix)
