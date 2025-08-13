(ns elyses-destructured-enchantments)

(defn first-card
  "Returns the first card from deck."
  [deck]
  (let [[first] deck] first))

(defn second-card
  "Returns the second card from deck."
  [deck]
  (let [[_ second] deck] second))

(defn swap-top-two-cards
  "Returns the deck with first two items reversed."
  [deck]
  (let [[first second & the-rest] deck]
    (apply conj [second first] the-rest)))

(defn discard-top-card
  "Returns a sequence containing the first card and
   a sequence of the remaining cards in the deck."
  [deck]
  (let [[first & the-rest] deck] [first the-rest]))

(def face-cards
  ["jack" "queen" "king"])

(defn insert-face-cards
  "Returns the deck with face cards between its head and tail."
  [deck]
  (let [[head & tail] deck]
    (flatten (remove nil? (vector head face-cards tail)))))