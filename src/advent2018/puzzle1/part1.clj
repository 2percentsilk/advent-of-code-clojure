;; https://adventofcode.com/2018/day/1
(ns advent2018.puzzle1.part1
  (:require [clojure.string :as str]))

(defn solve [input]
  (reduce + input))

; tests
(assert (= 6 (solve '(1 2 3))))

(def parsed-input
  (map read-string (str/split-lines (slurp "inputs/puzzle1"))))

(defn -main []
  (println (solve parsed-input)))
