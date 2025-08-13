(ns robot-name)

(def ^:private letters (map char (range 65 91)))
(def ^:private numbers (range 100 1000))

(def ^:private used-names (atom #{}))

(defn- generate-name []
  (let [potential-name (str
                        (rand-nth letters)
                        (rand-nth letters)
                        (rand-nth numbers))]
    (if (@used-names potential-name)
      (recur)
      (do
        (swap! used-names conj potential-name)
        potential-name))))

(defn robot []
  (atom {:name (generate-name)}))

(defn robot-name [robot]
  (@robot :name))

(defn reset-name [robot]
  (reset! robot {:name (generate-name)}))
