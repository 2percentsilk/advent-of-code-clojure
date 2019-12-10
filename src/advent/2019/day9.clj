(ns advent.2019.day7
  (:require [advent.2019.day5 :as base])
  (:require [clojure.math.combinatorics :as combo]))

(def puzzle-input (slurp "inputs/2019/day9"))

(defn run [program-input inputs]
  (-> program-input
      base/parse-input
      (base/build-state inputs)
      base/run-all
      :outputs))

; Part 1: 2204990589
#_(def part-1 
  (run puzzle-input '(1)))

; Part 2
#_(run puzzle-input '(2))
