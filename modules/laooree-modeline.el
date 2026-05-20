;; ----- Variables ----- ;;

(defvar-local laooree-modeline-accent-color (face-attribute 'mode-line-active :foreground)
  "Accent color to be used in the mode line.")



;; ----- Generic functions ------ ;;

(defun laooree-modeline--set-accent-color (accent-color)
  "Set the accent color of the laooree-modeline."
  (setq laooree-modeline-accent-color accent-color))


(defun laooree-modeline--with-indicator (inner-string bg-color text-color)
  "Returns `inner-string' inside an indicator with specified `bg-color'.
If non-nil, `text-color' specifies instring color."
  (let ((str (copy-sequence inner-string)))
    (if text-color
        (add-face-text-property 0 (length str) `(:background ,bg-color :foreground ,text-color) t str)
      (add-face-text-property 0 (length str) `(:background ,bg-color) t str))
    str)
  )


(defun laooree-modeline--buffer-status ()
  "Returns a glyph representing the buffer status."
  (unless (eq buffer-file-name nil)
    (cond ((eq buffer-read-only t) " ")
          ((buffer-modified-p) "󰙏 ")
          (t ""))))


(defun laooree-modeline--buffer-name ()
  "Returns the buffer name, in bold if the window is selected."
  (cond ((mode-line-window-selected-p) (propertize (buffer-name) 'face 'bold))
	("t" (buffer-name))))


(defun laooree-modeline--major-mode-name ()
  "Get a string with buffer major mode name, without `-mode' and `-'."
  (replace-regexp-in-string "-" " " (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode)))
  )


(defun laooree-modeline--major-mode-with-icons ()
  "Add icon to major-mode name, if relevant."
  (cond
   ((string= (laooree-modeline--major-mode-name) "bash ts")          "#! Bash (tree-sitter)")
   ((string= (laooree-modeline--major-mode-name) "bash")             "#! Bash")
   ((string= (laooree-modeline--major-mode-name) "c ts")             " C (tree-sitter)")
   ((string= (laooree-modeline--major-mode-name) "c")                " C")
   ((string= (laooree-modeline--major-mode-name) "dired")            "  Dired")
   ((string= (laooree-modeline--major-mode-name) "emacs lisp")       " Emacs Lisp")
   ((string= (laooree-modeline--major-mode-name) "haskell ts")       " Haskell (tree-sitter)")
   ((string= (laooree-modeline--major-mode-name) "haskell")          " Haskell")
   ((string= (laooree-modeline--major-mode-name) "latex ts")         " LaTeX (tree-sitter)")
   ((string= (laooree-modeline--major-mode-name) "LaTeX")            " LaTeX")
   ((string= (laooree-modeline--major-mode-name) "lisp interaction") " Lisp Interaction")
   ((string= (laooree-modeline--major-mode-name) "lua ts")           " Lua (tree-sitter)")
   ((string= (laooree-modeline--major-mode-name) "lua")              " Lua")
   ((string= (laooree-modeline--major-mode-name) "matlab ts")        " Matlab (tree-sitter)")
   ((string= (laooree-modeline--major-mode-name) "matlab")           " Matlab")
   ((string= (laooree-modeline--major-mode-name) "nix ts")           "󱄅 Nix (tree-sitter)")
   ((string= (laooree-modeline--major-mode-name) "nix")              "󱄅 Nix")
   ((string= (laooree-modeline--major-mode-name) "org")              " Org Mode")
   ((string= (laooree-modeline--major-mode-name) "python ts")        " Python (tree-sitter)")
   ((string= (laooree-modeline--major-mode-name) "python")           " Python")
   ((string= (laooree-modeline--major-mode-name) "text")             " Text")
   ((string= (laooree-modeline--major-mode-name) "typst ts")         " Typst (tree-sitter)")
   ((string= (laooree-modeline--major-mode-name) "typst")            " Typst")
   (t (capitalize (laooree-modeline--major-mode-name)))
   )
  )


(defun laooree-modeline--flymake-count (type)
  "Return count of current flymake reports of TYPE."
  (when (and (boundp 'flymake-mode) flymake-mode (mode-line-window-selected-p))
    (cl-loop for diag in (flymake-diagnostics)
             as diag-type = (flymake-diagnostic-type diag)
             count (eq (flymake--lookup-type-property diag-type 'severity)
		       (flymake--lookup-type-property type 'severity)))))



;; ----- Mode-line segments ----- ;;

(defun laooree-modeline--segment-buffer ()
  "Returns buffer name inside an indicator, maybe with an icon representing its status.
When window is not selected, omit the indicator."
  (if (mode-line-window-selected-p)
      (laooree-modeline--with-indicator (concat " "
						(laooree-modeline--buffer-status)
						(laooree-modeline--buffer-name)
						" ")
					laooree-modeline-accent-color
					(face-attribute 'default :background))
    (concat " "
	    (laooree-modeline--buffer-status)
	    (laooree-modeline--buffer-name)
	    " ")))


(defun laooree-modeline--segment-keyboard-macro ()
  "Returns an indicator signaling that a keyboard macro is being recorded."
  (if defining-kbd-macro (laoree-modeline--with-indicator "   Rec "
							  (face-attribute 'error :foreground)
							  (face-attribute 'default :background))
    ""))


(defun laooree-modeline--segment-major-mode ()
  "Returns an indicator for the major mode name in the mode-line."
  (propertize (format " %s" (laooree-modeline--major-mode-with-icons))
	      'face `(:foreground ,(face-attribute 'font-lock-comment-face :foreground)))
  )


(defun laooree-modeline--segment-flymake ()
  "Get `:error', `:warning', and `:note' flymake diagnostic count."
  (when (and (boundp 'flymake-mode) flymake-mode)
    (let ((error-count (laooree-modeline--flymake-count :error))
          (warning-count (laooree-modeline--flymake-count :warning))
          (note-count (laooree-modeline--flymake-count :note)))
      (concat (cond ((= error-count 0) (propertize (format " %s " error-count) 'face 'font-lock-comment-face))
                    (t (propertize (format " %s " error-count) 'face 'error)))
              (cond ((= warning-count 0) (propertize (format " %s " warning-count) 'face 'font-lock-comment-face))
                    (t (propertize (format " %s " warning-count) 'face 'warning)))
              (cond ((= note-count 0) (propertize (format "󱂻 %s" note-count) 'face 'font-lock-comment-face))
                    (t (propertize (format "󱂻 %s " note-count) 'face 'success)))))))


(defun laooree-modeline--segment-scroll-indicator ()
  "Indicate scroll position.
Shows current/total lines, or percentage when `display-line-numbers-mode' is active."
  (when (mode-line-window-selected-p)
    (let* ((indicator
            (cond
             ((use-region-p) (let ((selected-lines (count-lines (region-beginning) (region-end)))
                                            (selected-chars (- (region-end) (region-beginning))))
					(format " %dL %dC " selected-lines selected-chars)))
             (display-line-numbers-mode (let* ((value (format "%.0f" (/ (* 100.0 (point)) (point-max))))
                                               (percent (cond ((string= value "100") "Bot")
                                                              ((string= value "0")   "Top")
                                                              (t (concat value "%%")))))
                                          (concat " " percent " ")))
             (t (let ((total-lines (line-number-at-pos (point-max)))
                      (current-line (line-number-at-pos (point))))
                  (format " %d/%d " current-line total-lines))))))
      (laooree-modeline--with-indicator indicator
					laooree-modeline-accent-color
					(face-attribute 'default :background)))))


(defvar laooree-modeline-format
  '(""
    (:eval (laooree-modeline--segment-buffer))
    (:eval (laooree-modeline--segment-major-mode))
    (:eval (laooree-modeline--segment-keyboard-macro))
    mode-line-format-right-align
    (:eval (laooree-modeline--segment-scroll-indicator))
    )
  "Variable containing the `mode-line-format' specification for `laooree-modeline'.")



;; ----- Activation ----- ;;

(add-hook 'post-command-hook #'force-mode-line-update)
(setq-default mode-line-format laooree-modeline-format)

(provide 'laooree-modeline)
