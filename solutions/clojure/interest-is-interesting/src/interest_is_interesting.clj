(ns interest-is-interesting)

(defn interest-rate [balance]
  (cond (< balance 0) -3.213
        (< balance 1000) 0.5
        (< balance 5000) 1.621
        :else 2.475))

(defn calc-percent-value [percentage value]
  (-> percentage
      abs
      (/ 100.0)
      bigdec
      (* value)))

(defn annual-balance-update [balance]
  (-> balance
      interest-rate
      (calc-percent-value balance)
      (+ balance)))

(defn amount-to-donate [balance tax-free-percentage]
  (if (> 0 balance) 0
      (-> tax-free-percentage
          (calc-percent-value balance)
          (* 2.0)
          int)))