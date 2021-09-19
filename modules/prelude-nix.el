(defvar *enable-nix* t
  "Indicate that the system is managed by Nix, so that we can leverage Nix power. 

If it's enabled, some programming environments may benefit from nix-shell.")

(use-package nix-sandbox
  :commands (nix-shell-command nix-shell nix-compile nix-find-sandbox nix-current-sandbox nix-executable-find))

(defun nix/update-config ()
  (interactive)
  (save-buffer)
  (get-buffer-create "*nix*")
  (with-current-buffer "*nix*"
    (delete-region (point-min) (point-max))
    (start-process "nix" "*nix*" "nix-env" "-iA" "nixpkgs.myMac")
    (display-buffer "*nix*")
    (beginning-of-buffer)))

(defun nix/update-home ()
  (interactive)
  (save-buffer)
  (get-buffer-create "*nix*")
  (with-current-buffer "*nix*"
    (delete-region (point-min) (point-max))
    (start-process "nix" "*nix*" "home-manager" "switch")
    (display-buffer "*nix*")))

(use-package nix-mode
  :mode (".nix\\'" . nix-mode)
  :hook (nix-mode . yas-minor-mode)
  :bind (("C-c C-n" . nix/update-config)
         ("C-c C-m" . nix/update-home)))

(defun vterm-with-nix-shell ()
  "Start nix-shell here."
  (interactive)
  (let ((vterm (vterm "*nix-shell*")))
    (with-current-buffer vterm
      (vterm-send-string "nix-shell")
      (vterm-send-right))))

(provide 'prelude-nix)
