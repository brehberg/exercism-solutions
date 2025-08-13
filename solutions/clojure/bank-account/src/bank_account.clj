(ns bank-account)

(defn open-account []
  (ref {:open true :balance 0}))

(defn close-account [account]
  (dosync
   (alter account assoc :open false)))

(defn get-balance [account]
  (when (@account :open)
    (@account :balance)))

(defn update-balance [account change]
  (dosync
   (when (@account :open)
     (alter account assoc :balance
            (+ (@account :balance) change)))))
