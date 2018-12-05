;; https://adventofcode.com/2018/day/1

(ns puzzle1
  (:require [clojure.string :as str]))

(defn solve [input]
  (reduce + input))

; tests
(assert (= 6 (solve '(1 2 3))))

(def parsed-input
  (for
      [x (str/split (slurp "inputs/puzzle1") #"\n")]
      (read-string x)))

(defn -main []
  (println (solve parsed-input)))