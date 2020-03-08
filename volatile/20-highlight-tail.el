;; Credit: K. Scarlet
;; Copied from Emacs China Telegram Group Chat

(when nil
  (require 'highlight-tail)

  (setq highlight-tail-steps 40)
  (defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65"))
  (defvar blink-highlight-colors (list "#5D7E79" "#475E94" "#733780" "#808164"))
  (setq highlight-tail-colors '(("#5D7E79" . 0)))
  (highlight-tail-mode 1)
  (setq blink-highlight-fade-lists
        (mapcar (lambda (color)
                  (setq highlight-tail-colors `((,color . 0)))
                  (setq highlight-tail-colors-fade-list nil)
                  (setq highlight-tail-colors-with-100
                        (if (= (cdr (nth (1- (length highlight-tail-colors))
                                         highlight-tail-colors))
                               100)
                            highlight-tail-colors
                          (append highlight-tail-colors (list '(null . 100)))))
                  (highlight-tail-add-colors-fade-table 'default)
                  highlight-tail-colors-fade-list)
                blink-highlight-colors))
  (highlight-tail-get-colors-fade-table-with-key 'default)
  (setq blink-cursor-count 0)
  (defun blink-cursor-timer-function ()
    (when (not (internal-show-cursor-p))
      (when (>= blink-cursor-count (length blink-cursor-colors))
        (setq blink-cursor-count 0))
      (let ((color (nth blink-cursor-count blink-cursor-colors))
            (hl-color (nth blink-cursor-count blink-highlight-colors))
            (hl-color-list (nth blink-cursor-count blink-highlight-fade-lists)))
        (set-cursor-color color)
        (setq highlight-tail-colors `((,hl-color . 0)))
        (setq highlight-tail-colors-fade-list nil
              highlight-tail-nonhtfaces-bgcolors nil
              highlight-tail-const-overlays-list nil
              highlight-tail-update-const-overlays-to-this-list nil
              highlight-tail-face-max nil)
        (setq highlight-tail-colors-with-100
              (if (= (cdr (nth (1- (length highlight-tail-colors))
                               highlight-tail-colors))
                     100)
                  highlight-tail-colors
                (append highlight-tail-colors (list '(null . 100)))))
        (highlight-tail-add-colors-fade-table 'start)
        (highlight-tail-add-colors-fade-table 'default)
        (setq highlight-tail-colors-fade-list hl-color-list)
        (setq highlight-tail-face-max highlight-tail-steps)
        (highlight-tail-make-faces
         (highlight-tail-get-colors-fade-table-with-key 'default))
        (setq blink-cursor-count (+ 1 blink-cursor-count))))
    (internal-show-cursor nil (not (internal-show-cursor-p))))
  (define-globalized-minor-mode highlight-tail-global-mode highlight-tail-mode
    (lambda () (highlight-tail-mode 1)))
  (highlight-tail-global-mode 1))
