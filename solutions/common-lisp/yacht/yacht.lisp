(defpackage :yacht
  (:use :cl)
  (:export :score))
(in-package :yacht)

(defun score (dice category)
  "Returns the score of the dice for the given category."
  (case category
    (:ones            (sum-for-value dice 1))
    (:twos            (sum-for-value dice 2))
    (:threes          (sum-for-value dice 3))
    (:fours           (sum-for-value dice 4))
    (:fives           (sum-for-value dice 5))
    (:sixes           (sum-for-value dice 6))
    (:full-house      (check-full-house dice))
    (:four-of-a-kind  (check-four-of-a-kind dice))
    (:little-straight (check-straight dice '(1 2 3 4 5)))
    (:big-straight    (check-straight dice '(2 3 4 5 6)))
    (:yacht           (check-yacht dice))
    (:choice          (sum dice))
    (t 0)))

(defun check-yacht (dice)
  "a yacht is worth 50 points if all dice values are the same"
  (let ((sorted (sort dice #'<)))  
  (if (= (first sorted) (fifth sorted)) 50 0)))

(defun check-full-house (dice)
  "a full house is worth the sum of all dice if three of one value and two of another"
  (let ((sorted (sort dice #'<)))
  (if (and (or (and (= (first sorted) (second sorted))   ; (s, s, b, b, b) 
                    (= (third sorted) (fifth sorted)))   ;   or
               (and (= (first sorted) (third sorted))    ; (s, s, s, b, b)
                    (= (fourth sorted) (fifth sorted)))) ;  and not
           (not (= (first sorted) (fifth sorted))))      ; (d, d, d, d, d)
    (sum sorted)
    0)))

(defun check-four-of-a-kind (dice)
  "a four of a kind is worth the sum of the four dice showing the same face"
  (let ((sorted (sort dice #'<)))
  (cond    
    ((= (first sorted) (fourth sorted)) (* 4 (first sorted)))  ; (s, s, s, s, _)
    ((= (second sorted) (fifth sorted)) (* 4 (second sorted))) ; (_, b, b, b, b)
    (t 0))))

(defun check-straight (dice values)
  "a straight is worth 30 points if all dice values required are present"
  (if (equal values (sort dice #'<)) 30 0))

(defun sum-for-value (dice value)
  (sum (remove-if (lambda (die) (/= value die)) dice)))
  
(defun sum (dice)
  (reduce #'+ dice :initial-value 0))
