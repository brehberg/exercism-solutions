(defmodule leap
  (export (leap-year 1)))

(defun leap-year (year)  
  (andalso (divisible-by? year 4)
    (orelse (not (divisible-by? year 100))
      (divisible-by? year 400))))

(defun divisible-by? (year n) 
  (== 0 (rem year n)))