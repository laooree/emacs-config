;;; -*- lexical-binding: t -*-


;; ===== Useful generic functions ==================================================================
(defun wordel--string-without-element (str n)
  "Returns `str' without the `n'-th element (1-based)."
  (concat (seq-take str (- n 1)) (seq-drop str n)))


(defun wordel--string-replace (str n c)
  "Replace the `n'-th element (1-based) in `str' with `c'."
  (concat (seq-take str (- n 1)) c (seq-drop str n )))


(defun wordel--string-elt (str n)
  "Returns the `n'-th letter (1-based) in `str'."
  (format "%c" (seq-elt str (- n 1))))


(defun wordel--read-from-point (n)
  "Read `n' characters starting from `point'."
  (buffer-substring (point) (+ (point) n)))


;; ===== Shuffle candidate at point ================================================================
(defun wordel--shuffle-string (str &optional str-shuffled)
  "Returns a string by randomly shuffling `str'.
`str-shuffled' is used as an accumulator."
  (if (length> str 0)
      (let ((str-shuffled (or str-shuffled ""))
            (n (random (length str))))
	(wordel--shuffle-string (wordel--string-without-element str (+ n 1))
			       (concat (wordel--string-elt str (+ n 1)) str-shuffled)))
    str-shuffled))


(defun wordel-shuffle-candidate-at-point ()
  "Shuffles the candidate at point."
  (interactive)
  (let ((point-position (point)))
    (beginning-of-visual-line)
    (insert (wordel--shuffle-string (wordel--read-from-point 5)))
    (delete-char 5)
    (goto-char point-position)))


;; ===== Compute possible candidates from letters ==================================================
(defun wordel--list-candidates (letter allowed-positions candidate)
  "Returns a list of new candidates, obtained substituting `letter' in
`candidate', in its `allowed-positions'."
  (when allowed-positions
    (let* ((current (when (string= "_" (wordel--string-elt candidate (car allowed-positions)))
                      (wordel--string-replace candidate (car allowed-positions) letter)))
           (rest (wordel--list-candidates letter (cdr allowed-positions) candidate)))
      (if current
          (cons current rest)
        rest))))


(defun wordel--apply-hint-to-candidates (hint candidates)
  "Returns new candidates by applying `hint' to
 each of the `candidates' in the list."
  (when candidates
    (let* ((letter (car hint))
		   (allowed-positions (cdr hint))
		   (new-candidates (wordel--list-candidates letter allowed-positions (car candidates)))
		   (other-candidates (wordel--apply-hint-to-candidates hint (cdr candidates))))
	  (seq-concatenate 'list new-candidates other-candidates))))


(defun wordel--infer-from-hints (hints &optional candidates)
  (let ((candidates (or candidates '("_____"))))
    (if hints
	(wordel--infer-from-hints (cdr hints) (wordel--apply-hint-to-candidates (car hints) candidates))
    candidates)))


(defun wordel--build-hints ()
  "Interactively build a hints list by prompting for letters and their
 allowed positions. Enter an empty letter to finish."
  (let ((hints '()))
    (catch 'done
      (while t
        (let ((letter (read-string "Letter (empty to finish): ")))
          (when (string= "" letter)
            (throw 'done nil))
          (let* ((pos-str (read-string (format "Positions for '%s' (consecutive digits, 1-based): " letter)))
                 (positions (mapcar (lambda (ch) (- ch ?0))
                   (string-to-list pos-str))))
            (push `(,letter ,@positions) hints)))))
    (setq hints (nreverse hints))
    (message "Built hints: %S" hints)
    hints))


(defun wordel--get-printable-candidates-from-list (candidates)
  "Returns a string where `candidates' are separated with a newline."
  (when candidates
	(concat (car candidates) "\n" (wordel--get-printable-candidates-from-list (cdr candidates)))))


(defun wordel-insert-possible-candidates-at-point ()
  "Prompts the user for letters and their allowed positions,
 then inserts the resulting vaild candidates at point."
  (interactive)
  (let* ((hints      (wordel--build-hints))
		 (candidates (wordel--get-printable-candidates-from-list (wordel--infer-from-hints hints))))
	(insert candidates)))


;; ===== Management of possible letters ============================================================
(defun wordel-initialize-possible-letters ()
  "Clean current buffer, and initialize all the possible letters at the top.
Letters are inserted as in QWERTY layout."
  (interactive)
  (goto-char 0)
  (delete-char (- (point-max) 1))
  (insert ";; q w e r t y u i o p
;; a s d f g h j k l
;; z x c v b n m


")
  (goto-char (point-max)))


(defun wordel--remove-letter-from-possible-letters (letter)
  (goto-char (point-min))
  (let* ((search-limit (save-excursion (forward-line 3) (point)))
         (match-point (search-forward letter search-limit t)))
    (when match-point
      (goto-char (- match-point (length letter)))
      (if (eq ?\s (char-after match-point ))
          (delete-char 2)
        (progn (delete-char 1)
               (delete-char -1))))))


(defun wordel-update-possible-letters ()
  "Prompts for a word, and remove its letters from the possible letters at the
top, if they are present."
  (interactive)
  (let ((point-position (point))
        (letters-to-remove (mapcar #'string (string-to-list (read-string "Word to be removed: ")))))
    (mapc #'wordel--remove-letter-from-possible-letters letters-to-remove)
    (goto-char point-position)))


(provide 'wordel)
