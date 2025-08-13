(ns log-levels
  (:require [clojure.string :as str]))

(defn message [s]
  "Takes a string representing a log line
   and returns its message with whitespace trimmed."
  (-> s
      (str/split #":")
      (get 1)
      str/trim))

(defn log-level [s]
  "Takes a string representing a log line
   and returns its level in lower-case."
  (-> s
      (str/split #":")
      (get 0)
      (str/replace #"[\[\]]" "")
      str/lower-case))

(defn reformat [s]
  "Takes a string representing a log line and formats it
   with the message first and the log level in parentheses."
  (str (message s) " (" (log-level s) ")"))
