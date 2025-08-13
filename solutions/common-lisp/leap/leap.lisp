(defpackage :leap
  (:use :cl)
  (:export :leap-year-p))
(in-package :leap)

;; Returns whether 'year' is a leap year.
(defun leap-year-p (year)
  (labels ((is-divisible-p (n) 
    (zerop (mod year n))))
    
    (and (is-divisible-p 4) 
      (or (not (is-divisible-p 100))
        (is-divisible-p 400)))
  )
)
