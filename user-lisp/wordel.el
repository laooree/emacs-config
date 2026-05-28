;;; -*- lexical-binding: t -*-
(defun wordel--string-without-element (str n)
  "Returns `str' without the `n'-th element (1-based)."
  (concat (seq-take str (- n 1)) (seq-drop str n)))


(defun wordel--string-replace (str n c)
  "Replace the `n'-th element (1-based) in `str' with `c'."
  (concat (seq-take str (- n 1)) c (seq-drop str n )))


(defun wordel--string-elt (str n)
  "Returns the `n'-th letter (1-based) in `str'."
  (format "%c" (seq-elt str (- n 1))))


(defun wordel--shuffle-string (str &optional str-shuffled)
  "Returns a string by randomly shuffling `str'.
`str-shuffled' is used as an accumulator."
  (if (length> str 0)
      (let ((str-shuffled (or str-shuffled ""))
            (n (random (length str))))
	(wordel--shuffle-string (wordel--string-without-element str (+ n 1))
			       (concat (wordel--string-elt str (+ n 1)) str-shuffled)))
    str-shuffled))


(defun wordel--read-from-point (n)
  "Read `n' characters starting from `point'."
  (buffer-substring (point) (+ (point) n)))


(defun wordel-shuffle-candidate-at-point ()
  "Shuffles the candidate at point."
  (interactive)
  (let ((point-position (point)))
    (beginning-of-visual-line)
    (insert (wordel--shuffle-string (wordel--read-from-point 5)))
    (delete-char 5)
    (goto-char point-position)))


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
          (let* ((pos-str (read-string (format "Positions for '%s' (space-separated, 1-based): " letter)))
                 (positions (mapcar #'string-to-number
                                    (split-string pos-str nil t))))
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


(provide 'wordel)
