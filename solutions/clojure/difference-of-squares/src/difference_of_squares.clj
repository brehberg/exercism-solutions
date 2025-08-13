(ns difference-of-squares)

(defn sum-of-squares
  "Calculate sum of squares from 1 to a given end number
      y = (x * (x+1) * (2x+1)) / 6"
  [num]
  (/ (* num (inc num) (inc (* 2 num))) 6))

(defn square-of-sum
  "Calculate square of sum from 1 to a given end number
      y = (x * (x+1) / 2)^2"
  [num]
  (let [sum (/ (* num (inc num)) 2)] (* sum sum)))

(defn difference
  "Calculate difference between sum of squares and square of sum"
  [num]
  (- (square-of-sum num) (sum-of-squares num)))
