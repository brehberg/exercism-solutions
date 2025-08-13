(ns interest-is-interesting)

(defn interest-rate [balance]
  (cond (< balance 0) -3.213
        (< balance 1000) 0.5
        (< balance 5000) 1.621
        :else 2.475))

(defn absolute-value [n] (max n (- n)))

(defn calc-percent-value [value percentage]
  (-> percentage
      absolute-value
      (/ 100.0)
      bigdec
      (* value)))

(defn annual-balance-update [balance]
  (->> balance
       interest-rate
       (calc-percent-value balance)
       (+ balance)))

(defn amount-to-donate [balance tax-free-percentage]
  (if (> 0 balance) 0
      (-> balance
          (calc-percent-value tax-free-percentage)
          (* 2.0)
          (int))))