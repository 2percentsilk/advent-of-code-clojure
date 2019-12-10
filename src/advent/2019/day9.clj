(ns advent.2019.day9
  (:require [advent.2019.day5 :as base]))

(def puzzle-input (slurp "inputs/2019/day9"))

(defn run [program-input inputs]
  (-> program-input
      base/parse-input
      (base/build-state inputs)
      base/run-all-iter
      :outputs))

; Part 1: 2204990589
#_(def part-1 
  (run puzzle-input '(1)))

; Part 2: 50008
#_(def part-2
    (run puzzle-input '(2)))
