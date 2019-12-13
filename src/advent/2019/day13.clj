(ns advent.2019.day13
  (:require [advent.2019.day5 :as base])
  (:require [clojure.set :as set]))

(def puzzle-input (slurp "inputs/2019/day13"))

(defn outputs [program-input inputs]
  (-> program-input
      base/parse-input
      (base/build-state inputs)
      base/run-all-iter
      :outputs
      reverse))

(defn blocks [id]
  (->> (outputs puzzle-input '())
       (partition 3)
       (map vec)
       (filter #(= (get % 2) id))
       (map #(vector (get % 0) (get % 1)))
       set))

; Part 1: 427
#_(def part-1 (count (blocks 2)))
