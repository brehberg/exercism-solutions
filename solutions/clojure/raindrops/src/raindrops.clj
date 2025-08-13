(ns raindrops)

(defn convert
  "Convert a number into its corresponding raindrop sounds."
  [number]
  (letfn [(divisible-by? [d] (zero? (mod number d)))
          (factor [num, drop] (when (divisible-by? num) drop))]
    (let [sound (str (factor 3 "Pling") (factor 5 "Plang") (factor 7 "Plong"))]
      (if (empty? sound) (str number) sound))))
