(ns cars-assemble)

(def base-rate 221)
(defn success-rate [speed] (cond (= speed 10) 0.77
                                 (= speed 9) 0.80
                                 (>= speed 5) 0.90
                                 (>= speed 1) 1.00
                                 :else 0.00))

(defn production-rate
  "Returns the assembly line's production rate per hour,
   taking into account its success rate"
  [speed]
  (* (* speed base-rate) (success-rate speed)))

(defn working-items
  "Calculates how many working cars are produced per minute"
  [speed]
  (int (/ (production-rate speed) 60)))
