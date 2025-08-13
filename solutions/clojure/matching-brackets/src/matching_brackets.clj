(ns matching-brackets)

(def opener #{\[, \{, \(})
(def closer #{\], \}, \)})
(def pairs {\[ \], \{ \}, \( \)})

(defn- closers-match? [remaining closers]
  (let [[next & the-rest] remaining]
    (cond
      ;; if no characters are left, check that no more closers are expected
      (empty? remaining) (empty? closers)
      ;; if next character is a closer, check it matches the expected closer
      (contains? closer next) (let [[expected & the-others] closers]
                                (if (= next expected) (closers-match? the-rest the-others) false))
      ;; if next character is an opener, add corresponding value to closers
      (contains? opener next) (closers-match? the-rest (-> pairs (get next) (cons closers)))
      ;; otherwise, check the rest of the characters
      :else (closers-match? the-rest closers))))

(defn valid?
  "Checks that all the brackets and braces in the input are matched correctly, and nested correctly"
  [input]
  (-> input seq (closers-match? [])))