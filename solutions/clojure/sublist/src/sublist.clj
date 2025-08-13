(ns sublist)

(defn- sublist?
  [a b]
  (cond (> (count a) (count b)) false
        (= a (take (count a) b)) true
        :else (sublist? a (rest b))))

(defn classify
  "Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list."
  [list1 list2]
  (cond (and (< (count list1) (count list2))
             (sublist? list1 list2)) :sublist
        (and (> (count list1) (count list2))
             (sublist? list2 list1)) :superlist
        (= list1 list2) :equal
        :else :unequal))
