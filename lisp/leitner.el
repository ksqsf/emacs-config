;;; leitner.el --- Simple Leitner system for Emacs   -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Requires: (cl-lib seq ht)

;;; Commentary:

;; The Leitner system [1] is a simple method to help users to quickly
;; memorize a large number of boring information stored in the format
;; of flashcards.

;; To use leitner.el, you first need to define some decks with
;; `leitner-defdeck'.  At the bottom of this file are some examples
;; you can follow.

;; \\[leitner] lists the known decks and some basic information about
;; the deck.  Press `RET' on the line to start reviewing the deck.

;; In a review, the card is displayed in two or three lines:

;; - Question: the question
;; - Truth: the truth
;; - Answer: your answer

;; If you see a new card, the truth line will be printed.  Your answer
;; is expected to match the truth, regardless of whether Truth is
;; shown to you.

;; If you successfully answer the question, this card will be shown
;; less frequently.  The more you get it right, the longer the review
;; interval will be.  However, if you fail to recall the correct
;; answer, the learning progress will be reset.

;; To save your learning progress, use \\[leitner-save].  To load your
;; saved progress, use \\[leitner-load].

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'ht)

(defgroup leitner
  nil
  "Customization group for the Leitner system.")

(defcustom leitner-decks-file (expand-file-name "var/leitner.eld" user-emacs-directory)
  "The file where decks are stored.")


(cl-defstruct (leitner-card (:type vector))
  "A Leitner card."
  question answer (success-count -1))

(cl-defstruct (leitner-deck (:type vector))
  "A Leitner deck (a collection of cards)."
  name cards)

(defvar leitner-decks (make-hash-table)
  "Known decks in the Leitner system.")

(defvar leitner-updated nil
  "Non-nil if there is new progress.")

;;;###autoload
(defmacro leitner-defdeck (id name &rest plist)
  "Define a Leitner deck with the id ID and human-readable name NAME."
  (declare (indent 2))
  (let* ((cards (cl-loop for (q a) on plist by 'cddr
                         vconcat (vector (make-leitner-card :question q :answer a))))
         (deck (make-leitner-deck :name name :cards cards)))
    (puthash id deck leitner-decks)))

(defun leitner-get-deck (id)
  "Get the deck with id ID."
  (gethash id leitner-decks))

(defun leitner-number-of-learned-cards (deck)
  "Compute the number of cards displayed at least once."
  (let ((cards (leitner-deck-cards deck)))
    (length (seq-filter (lambda (card) (> (leitner-card-success-count card) -1)) cards))))

(defun leitner-number-of-cards (deck)
  (length (leitner-deck-cards deck)))

(defun leitner-deck-next-card (deck)
  (aref (leitner-deck-cards deck) 0))

(defun leitner-deck-move-after (deck n)
  "Destructively move the first card to N cards after it."
  (setq n (min (1- (leitner-number-of-cards deck)) n))
  ;; destruct cards into [cur, ...n cards, ...rest]
  ;; the result is [...n cards, cur, ...rest]
  (let* ((cards (leitner-deck-cards deck))
         (cur (aref cards 0)))
    (cl-loop for i from 1 to n
             do (aset cards (1- i) (aref cards i)))
    (aset cards n cur)))

;;;###autoload
(defun leitner-save ()
  "Save the learning progress."
  (interactive)
  (with-temp-buffer
    (insert (prin1-to-string leitner-decks))
    (write-file leitner-decks-file))
  (leitner-clear-modified))

;;;###autoload
(defun leitner-load ()
  "Load the learning progress."
  (interactive)
  (when leitner-updated
    (user-error "Refuse to load because you haven't saved your progress so far!"))
  (condition-case err
      (progn
        (setq saved-decks (read (with-temp-buffer
                                  (insert-file-contents leitner-decks-file)
                                  (buffer-string))))
        (setq leitner-decks (ht-merge leitner-decks saved-decks))
        (leitner-clear-modified)
        (leitner-revert-menu)
        (message "Loading succeeded; there are %d known decks" (hash-table-count leitner-decks)))
    ('error (message "Couldn't load from the decks file due to %s" err))))


(defvar leitner-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'leitner-review)
    (define-key map (kbd "s") 'leitner-save)
    (define-key map (kbd "l") 'leitner-load)
    (define-key map (kbd "C-x C-s") 'leitner-save)
    (define-key map (kbd "C-x C-l") 'leitner-load)
    map))

(defun leitner--menu-entries ()
  (let ((ids (hash-table-keys leitner-decks))
        (format-fn #'(lambda (id)
                       (let ((deck (leitner-get-deck id)))
                         (list id (vector (leitner-deck-name deck)
                                          (prin1-to-string (leitner-number-of-cards deck))
                                          (prin1-to-string (leitner-number-of-learned-cards deck))))))))
    (mapcar format-fn ids)))

(define-derived-mode leitner-menu-mode tabulated-list-mode "leitner-menu-mode"
  "Major mode for interacting with the Leitner system."
  (setq tabulated-list-format [("Deck name" 20 t)
                               ("#Cards" 10 t)
                               ("#Learned" 10 t)])
  (setq tabulated-list-entries 'leitner--menu-entries)
  (tabulated-list-init-header)
  (tabulated-list-print t))

;;;###autoload
(defun leitner ()
  "Start the Leitner system."
  (interactive)
  (let ((buf (get-buffer-create "*Leitner Menu*")))
    (with-current-buffer buf
      (leitner-menu-mode))
    (display-buffer buf)))

(defun leitner-review (&optional deck-id)
  (interactive)
  (setq deck-id (or deck-id (tabulated-list-get-id)))
  (let* ((deck (leitner-get-deck deck-id))
         (buf (get-buffer-create (format "*Leitner Review: %s*" (leitner-deck-name deck)))))
    (with-current-buffer buf
      (leitner-review-mode)
      (setq-local leitner-deck deck)
      (leitner-review-show-next-card))
    (display-buffer buf)))

(defmacro leitner-eval-in-menu-buffer-when-its-alive (&rest forms)
  `(let ((buf (get-buffer "*Leitner Menu*")))
     (when (buffer-live-p buf)
       (with-current-buffer buf
         ,@forms))))

(defun leitner-revert-menu ()
  (leitner-eval-in-menu-buffer-when-its-alive
   (revert-buffer)))

(defun leitner-set-modified ()
  (setq leitner-updated t))

(defun leitner-clear-modified ()
  (setq leitner-updated nil))


(defvar leitner-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'leitner-review-check)
    (define-key map (kbd "C-j") 'leitner-review-check)
    (define-key map (kbd "C-x C-s") 'leitner-save)
    map))

(define-derived-mode leitner-review-mode fundamental-mode "leitner-review-mode"
  "The major mode for reviewing a Leitner deck."
  (setq-local leitner-deck nil)
  (setq-local leitner-card nil)
  (setq-local leitner-streak 0)
  (setq-local leitner-number-of-reviews 0)
  (setq-local leitner-answer nil))

(defun leitner--review-insert-internals (deck)
  (cl-loop for card being the elements of (leitner-deck-cards deck)
           do (insert (format "- %s\t%s\t%d\n"
                              (leitner-card-question card)
                              (leitner-card-answer card)
                              (leitner-card-success-count card)))))


(defun leitner-review-show-next-card ()
  "Find the next scheduled card and insert it into the review buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (insert (format "progress: %d out of %d\n" (leitner-number-of-learned-cards leitner-deck) (leitner-number-of-cards leitner-deck)))
  (insert (format "  streak: %d out of %d\n" leitner-streak leitner-number-of-reviews))
  ;; (leitner--review-insert-internals leitner-deck)
  (let ((card (leitner-deck-next-card leitner-deck)))
    (setq-local leitner-card card)
    (insert (format "Question: %s\n" (leitner-card-question card)))
    (when (= -1 (leitner-card-success-count card))
      (insert "   Truth: " (leitner-card-answer card) "\n")))
  (insert "  Answer: ")
  (cl-incf leitner-number-of-reviews)
  (leitner-revert-menu)
  (put-text-property (1- (point-max)) (point-max) 'rear-nonsticky t)
  (put-text-property (point-min) (point-max) 'intangible t)
  (put-text-property (point-min) (point-max) 'read-only t)
  (set-buffer-modified-p nil)
  (goto-char (point-max)))

(defun leitner-review-get-answer ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^  Answer: " nil nil)
    (buffer-substring (point) (pos-eol))))

(defun leitner-review-check ()
  (interactive)
  (let ((ground-truth (leitner-card-answer leitner-card)))
    (if (string= ground-truth (leitner-review-get-answer))
        (leitner-review-succeed)
      (leitner-review-fail)))
  (leitner-set-modified)
  (leitner-review-show-next-card))

(defun leitner-review-succeed ()
  (message "You are right!")
  (cl-incf leitner-streak)
  (cl-incf (leitner-card-success-count leitner-card))
  (let ((n (leitner-card-success-count leitner-card)))
    (leitner-deck-move-after leitner-deck n)))

(defun leitner-review-fail ()
  (message "You are wrong!")
  (setq leitner-streak 0)
  ;; No need to relearn.
  (setf (leitner-card-success-count leitner-card) -1))


(leitner-defdeck
    circle-of-fifths "Cycle of Fifths"
  "F" "1b"
  "C" "0"
  "G" "1s"
  "D" "2s"
  "A" "3s"
  "E" "4s"
  "B" "5s"
  "Gb" "6b"
  "Db" "5b"
  "Ab" "4b"
  "Eb" "3b"
  "Bb" "2b")

(leitner-defdeck
    huma "Huma"
  "一" "fi"
  "二" "me")

(provide 'leitner)
;;; leitner.el ends here
