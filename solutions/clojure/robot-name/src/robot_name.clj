(ns robot-name)

(def letters (map char (range 65 91)))
(def numbers (range 100 1000))

(defn- generate-name []
  (str
   (rand-nth letters)
   (rand-nth letters)
   (rand-nth numbers)))

(defn robot []
  (atom {:name (generate-name)}))

(defn robot-name [robot]
  (@robot :name))

(defn reset-name [robot]
  (reset! robot {:name (generate-name)}))
