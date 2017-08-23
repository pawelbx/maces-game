;;; -*- lexical-binding: t -*-

(defvar anagram-state nil "Holds all game state")

(defface anagram-letters-face
  '((t :foreground "#859900"
       :weight bold
       :height 1.5))
  "face for shuffled letters"
  :group 'anagram)

(defface anagram-guess-face
  '((t :foreground "#268bd2"
       :weight bold
       :height 1.5))
  "face for user input"
  :group 'anagram)

(defface anagram-points-face
  '((t :foreground "#2aa198"
       :weight bold
       :height 1.5))
  "face for points"
  :group 'anagram)

(defface anagram-message-face
  '((t :foreground "#6c71c4"
       :weight bold
       :height 1.5))
  "face for messages"
  :group 'anagram)

(defface anagram-instruction-face
  '((t :foreground "#657b83"
       :weight bold
       :height 1.1))
  "face for messages"
  :group 'anagram)

(defun anagram ()
  "Nice anagram game"
  (interactive)
  (switch-to-buffer "anagram")
  (anagram-mode)
  (anagram-init-game))

(define-derived-mode anagram-mode special-mode "anagram"
  (define-key anagram-mode-map (kbd "SPC") 'anagram-rotate-letters)
  (define-key anagram-mode-map (kbd "RET") 'anagram-check-guess)
  (define-key anagram-mode-map (kbd "DEL") 'anagram-delete-letter)
  (define-key anagram-mode-map (kbd "Q") 'anagram-quit)
  (--map (anagram-define-letter-key it) '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
                                          "k" "l" "m" "n" "o" "p""q" "r" "s" "t"
                                          "u" "v" "w" "x" "y" "z")))

(defun anagram-check-guess()
  "checks if guess if correct"
  (interactive)
  (let* ((guess (anagram-get-user-input))
        (words (nth 1 anagram-state)))
    (if (member guess (anagram-get-found))
        (anagram-set-msg "Already found that word")
      (if (--first (equal guess it) words)
          (progn
            (anagram-add-to-found guess)
            (anagram-set-msg "Nice!")
            (anagram-add-points guess))
        (if (< (length guess) 4)
            (anagram-set-msg "At least 4 letter needed")
          (anagram-set-msg "Not Found"))))
    (anagram-clear-user-input)
    (anagram-render)))

(defun anagram-quit ()
  "kill current buffer"
  (interactive)
  (kill-buffer (current-buffer)))


(defun anagram-add-to-found (word)
  "gets words that have been found"
    (setcar (nthcdr 5 anagram-state) (cons word (anagram-get-found))))

(defun anagram-get-found ()
  "gets words that have been found"
  (nth 5 anagram-state))

(defun anagram-get-user-input ()
  "gets the current input"
  (nth 2 anagram-state))

(defun anagram-clear-user-input ()
  "clears user input"
  (setcar (nthcdr 2 anagram-state) ""))

(defun anagram-set-msg (msg)
  "sets message to user"
  (setcar (nthcdr 4 anagram-state) msg))

(defun anagram-get-msg ()
  "gets current message to user"
  (nth 4 anagram-state))

(defun anagram-delete-letter ()
  "delete letter from guess"
  (interactive)
  (setcar (nthcdr 2 anagram-state)
          (substring (nth 2 anagram-state) 0 (1- (length (nth 2 anagram-state)))))
  (anagram-render))

(defun anagram-get-points ()
  "get current points"
  (nth 3 anagram-state))

(defun anagram-add-points (word)
  "add points dependent of the length of the word"
  (let ((points (nthcdr 3 anagram-state))
        (wlen (length word))
        (num-points (anagram-get-points)))
    (cond ((equal wlen 4) (setcar points (+ num-points 2)))
          ((< wlen 6) (setcar points (+ num-points 4)))
          ((< wlen 8) (setcar points  (+ num-points 6)))
          ((< wlen 10) (setcar points (+ num-points 8)))
          (t (setcar points (+ num-points 10))))))

(defun anagram-define-letter-key (letter)
  (define-key anagram-mode-map (kbd letter)
    (lambda ()
      "check if key is valid"
      (interactive)
      (when (anagram-str-contains? (car (coerce letter 'list)) (car anagram-state))
        (setcar (nthcdr 2 anagram-state)
                (concat (nth 2 anagram-state) letter))
        (anagram-render)))))

(defun anagram-init-game ()
  (setq anagram-state (anagram-generate))
  (anagram-render))

(defun anagram-render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (car anagram-state) 'face 'anagram-letters-face))
    (insert (propertize (format "\t\t POINTS: %d" (anagram-get-points))
                        'face 'anagram-points-face))
    (insert "\n\n")
    (insert (propertize (anagram-get-msg) 'face 'anagram-message-face))
    (insert "\n\n")
    (insert (propertize (concat "Make words from the letters above.\n"
                                "You can use the same letter multiple times in your word.\n"
                                "Try to get to at least 21 points!")))
    (insert "\n\n")
    (insert (propertize (concat "Space to rotate letters\n"
                                "Q to quit\n"
                                "Enter to submit your guess")))
    (insert "\n\n")
    (insert (propertize (nth 2 anagram-state) 'face 'anagram-guess-face))))

(defun anagram-rotate-letters ()
  "shuffle letters"
  (interactive)
  (setcar anagram-state (coerce (-rotate 1 (coerce (car anagram-state) 'list)) 'string))
  (anagram-render))

(defun anagram-generate()
  (require 'dash)
  (let* ((words (anagram-load-words))
         (word (generate-anagram-letters words))
         (anagrams
          (-filter (lambda (curr-word)
                     (-reduce-from (lambda (mem letter)
                                     (and mem (anagram-str-contains? letter word)))
                                   t (coerce curr-word 'list)))
                   words)))
    ;; scrambled word, anagrams, user input, points, current msg, found words
    (list (coerce (anagram-shuffle (delete-dups (coerce word 'list))) 'string)
          anagrams "" 0 "" '())))

(defun anagram-load-words ()
  (with-current-buffer (find-file-noselect "words-alpha.txt")
    (split-string
     (save-restriction
       (widen)
       (buffer-substring-no-properties
        (point-min)
        (point-max)))
     "\n" t)))

(defun anagram-generate-letters (words)
  (let ((valid-words (--filter (equal (length (delete-dups (coerce it 'list))) 7) words)))
    (nth (random (length valid-words)) valid-words)))

(defun anagram-str-contains? (needle s)
  (if (member needle (coerce s 'list)) t nil))

(defun anagram-shuffle (list)
  (let ((shuff-list (-copy list))
        (i (- (length list) 1)))
    (while (> i 0)
      (let ((j (random (+ i 1)))
            (elem (nth i shuff-list)))
        (setcar (nthcdr i shuff-list) (nth j shuff-list))
        (setcar (nthcdr j shuff-list) elem)
        (setq i (- i 1))))
    shuff-list))

(provide 'anagram-game)

;;; anagram-game.el ends here
