;; https://adventofcode.com/2018/day/3
(ns puzzle3.part1
  (:require [clojure.string :as str]))

(defn claim-regex [x]
  (map read-string (drop 1 (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" x))))

(assert (= '(123 30 2 5 4) (claim-regex "#123 @ 30,2: 5x4")))

(defn parse-claim [x]
  (let [[id left-start top-start width height] (claim-regex x)]
    (vector id left-start top-start (- (+ left-start width) 1) (- (+ top-start height) 1))))

(assert (= [123 3 2 7 5] (parse-claim "#123 @ 3,2: 5x4")))

(defn is-in-bounds [row-index top bottom]
  (and (>= row-index top) (<= row-index bottom)))

(defn mark-cell-by-claim [col-index cell-value left right]
  (if (is-in-bounds col-index left right)
    (+ cell-value 1)
    cell-value))

(assert (= 0 (mark-cell-by-claim 0 0 1 1)))
(assert (= 1 (mark-cell-by-claim 1 0 1 1)))

(defn mark-row-by-claim [row-index row claim-bounds]
  (let [[claim-id left top right bottom] claim-bounds]
    (if (is-in-bounds row-index top bottom)
      (vec (map-indexed (fn [index cell] (mark-cell-by-claim index cell left right)) row))
      row
    )
  ))

(assert (= [0 0] (mark-row-by-claim 0 [0 0] [0 1 1 1 1])))
(assert (= [0 1] (mark-row-by-claim 1 [0 0] [1 1 1 1 1])))

(defn claim-reducer [fabric-state claim-bounds]
  (vec (map-indexed
    (fn [index row] (mark-row-by-claim index row claim-bounds))
    fabric-state)))

(assert (= [[0 0] [0 1]] (claim-reducer [[0 0] [0 0]] [0 1 1 1 1])))

(defn initial-fabric-state [width height]
  (vec (replicate height (vec (replicate width 0)))))

(assert (= [[0 0] [0 0]] (initial-fabric-state 2 2)))
(assert (= [[0] [0]] (initial-fabric-state 1 2)))

(defn claimed-fabric-state [fabric-width fabric-height claims]
  (vec (reduce claim-reducer (initial-fabric-state fabric-width fabric-height) claims)))

(defn overlaps-in-row [state-row]
  (count (filter (fn [x] (> x 1)) state-row)))

(defn count-overlaps [fabric-state]
  (reduce + (map overlaps-in-row fabric-state)))

(defn overlapping-claim-squares [fabric-width fabric-height claims]
  (count-overlaps
    (claimed-fabric-state fabric-width fabric-height claims)))

(def test-claim-strings
  '("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))

(assert (= 4 (overlapping-claim-squares 8 8 (map parse-claim test-claim-strings))))

(defn claim-width [claim]
  (get claim 3))

(defn claim-height [claim]
  (get claim 4))

(defn solve [claim-strings]
  (let [claims (map parse-claim claim-strings)
        claim-widths (map claim-width claims)
        claim-heights (map claim-height claims)]
    (overlapping-claim-squares
      (apply max claim-widths)
      (apply max claim-heights)
      claims)
  ))

(assert (= 4 (solve test-claim-strings)))

(def parsed-input
  (str/split-lines (slurp "inputs/puzzle3")))

(defn -main [] (println (solve parsed-input)))
