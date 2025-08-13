(ns reverse-string)

(defn reverse-string [s]
  (.toString (.reverse (StringBuilder. s))))
