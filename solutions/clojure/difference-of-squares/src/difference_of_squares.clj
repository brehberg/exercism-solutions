(ns difference-of-squares)

(defn- pow
  "Returns integer number of x to the power of n"
  [x n]
  (reduce * (repeat n x)))

(defn sum-of-squares
  "Calculate sum of squares from 1 to a given end number."
  [num]
  (/ (+ num (* 3 (pow num 2)) (* 2 (pow num 3))) 6))

(defn square-of-sum
  "Calculate square of sum from 1 to a given end number."
  [num]
  (pow (/ (+ num (pow num 2)) 2) 2))

(defn difference
  "Calculate difference between sum of squares and square of sum."
  [num]
  (- (square-of-sum num) (sum-of-squares num)))
