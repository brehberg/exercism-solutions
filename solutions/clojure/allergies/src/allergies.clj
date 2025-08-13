(ns allergies)

(defn allergies [score]
  (cond-> []
    (bit-test score 0) (conj :eggs)
    (bit-test score 1) (conj :peanuts)
    (bit-test score 2) (conj :shellfish)
    (bit-test score 3) (conj :strawberries)
    (bit-test score 4) (conj :tomatoes)
    (bit-test score 5) (conj :chocolate)
    (bit-test score 6) (conj :pollen)
    (bit-test score 7) (conj :cats)))

(defn allergic-to? [score allergen]
  (some #{allergen} (allergies score)))
