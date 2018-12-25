(ns advent.2018.day25
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(def puzzle-input
  (->> "inputs/2018/day25"
       slurp
       str/split-lines))

(def test-input
  '("0,0,0,0"
    "3,0,0,0"
    "0,3,0,0"
    "0,0,3,0"
    "0,0,0,3"
    "0,0,0,6"
    "9,0,0,0"
    "12,0,0,0"))

(defn parse-coordinate [input]
  (mapv read-string (str/split input #",")))

(defn parse-input [input]
  (->> input
       (map parse-coordinate)
       set))

(defn distance [[x0 y0 z0 t0] [x1 y1 z1 t1]]
  (+ (Math/abs (- x0 x1))
     (Math/abs (- y0 y1))
     (Math/abs (- z0 z1))
     (Math/abs (- t0 t1))))

(defn constellations [input]
  (loop [remaining input
         current-cons #{}
         current-starting #{(first input)}
         all-cons #{}]
    (cond
      (empty? remaining) (conj all-cons current-cons)
      (empty? current-starting) (recur remaining #{}
                                       #{(first remaining)}
                                       (conj all-cons current-cons))
      :else (let [new-coord (first current-starting)
                  neighbours (set (filter #(<= (distance % new-coord) 3) remaining))]
              (recur (set/difference remaining neighbours)
                     (set/union current-cons neighbours)
                     (set/union (disj current-starting new-coord) neighbours)
                     all-cons)))))

; Part 1
#_(count (constellations (parse-input puzzle-input)))

; Part 2
