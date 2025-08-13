(ns anagram
  (:require [clojure.string :as str]))

(defn- is-anagram?
  [base candidate]
  (and (= (count base) (count candidate))
       (let [word (str/lower-case candidate)]
         (and (not= base word)
              (= (sort base) (sort word))))))

(defn anagrams-for
  "Returns all candidates that are anagrams of, but not equal to, 'word'"
  [word candidates]
  (let [base (str/lower-case word)]
    (filter (partial is-anagram? base) candidates)))
