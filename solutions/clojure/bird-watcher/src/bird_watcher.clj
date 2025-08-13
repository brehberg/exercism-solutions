(ns bird-watcher)

(def last-week
  [0 2 5 3 7 8 4])

(defn today [birds]
  (last birds))

(defn inc-bird [birds]
  (conj (pop birds) (inc (today birds))))

(defn day-without-birds? [birds]
  (boolean (seq (filter #(= 0 %) birds))))

(defn n-days-count [birds n]
  (reduce + 0 (subvec birds 0 n)))

(defn busy-days [birds]
  (count (filter #(<= 5 %) birds)))

(def odd-pattern (cycle [1 0]))
(defn odd-week? [birds]
  (= birds odd-pattern))
