(defpackage :raindrops
  (:use :cl)
  (:export :convert))

(in-package :raindrops)

(defun convert (n)
  "Converts a number to a string of raindrop sounds."
  (let
    ((result 
      (format nil "~[Pling~]~[Plang~]~[Plong~]" 
        (rem n 3) (rem n 5) (rem n 7))
    ))
    (if (zerop (length result))
      (write-to-string n) result)
  )
)
