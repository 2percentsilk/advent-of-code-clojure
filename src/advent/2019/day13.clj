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

(defn update-state-data [{data :data :as state} k v]
  (assoc state :data  (assoc data k v)))

(defn start-state [program-input inputs start-key start-val]
  (-> program-input
      base/parse-input
      (base/build-state inputs)
      (update-state-data start-key start-val)))

(defn blocks [program-outputs id]
  (->> program-outputs
       (partition 3)
       (map vec)
       (filter #(= (get % 2) id))
       (map #(vector (get % 0) (get % 1)))
       set))

; Part 1: 427
#_(def part-1 
    (-> puzzle-input
        (outputs '())
        (blocks 2)
        count))

(defn run-as-long-input [state]
  (loop [iter-state state]
    (if (empty? (:inputs iter-state)) iter-state
        (recur (base/run-once iter-state)))))

(-> (start-state puzzle-input '(0 -1) 0 2)
    run-as-long-input
    :outputs
    reverse
    (blocks 4))
