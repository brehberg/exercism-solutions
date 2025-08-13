(ns squeaky-clean
  (:require [clojure.string :as str]))

(defn clean
  "a partial set of routines to help a developer clean up identifier names"
  [input]
  (-> input
      ;; Replace any spaces encountered with underscores
      (str/replace #"\s" "_")
      ;; Replace control characters with the upper case string "CTRL"
      (str/replace #"\p{Cntrl}" "CTRL")
      ;; Convert kebab-case to camelCase
      (str/replace #"(-\p{L})" #(str/upper-case (%1 1)))
      ;; Omit characters that are not letters
      (str/replace #"[^\p{L}_]" "")
      ;; Omit Greek lower case letters
      (str/replace #"[α-ω]" "")))
