(ns leap)

(defn leap-year?
  "Returns whether 'year' is a leap year."
  [year]

  (letfn [(divisible-by? [n]
            (zero? (mod year n)))]

    (and (divisible-by? 4)
         (or (not (divisible-by? 100))
             (divisible-by? 400)))))
