(ns leap)

(defn- divisible-by?
  "Return wheter x is evenly divisible by n"
  [x n]
  (= (mod x n) 0))

(defn leap-year?
  "Returns whether 'year' is a leap year."
  [year]
  (or (divisible-by? year 400)
      (and (divisible-by? year 4)
           (not (divisible-by? year 100)))))
