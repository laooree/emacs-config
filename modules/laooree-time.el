;; Author: laooree
;; Define some useful stuff to work with time/timers.


;; Return current time as (ticks . hz)
(setq current-time-list nil)

(defun laooree-time-today-seconds ()
  "Return current seconds passed since today's 00:00:00."
  (% (/ (car (current-time)) (cdr (current-time))) (* 24 60 60)))


(provide 'laooree-time)
