;; https://adventofcode.com/2018/day/6
(ns puzzle6.part1
  (:require [clojure.string :as str]))

(defn left-bound [inputs]
  (apply min (map first inputs)))

(defn right-bound [inputs]
  (apply max (map first inputs)))

(defn top-bound [inputs]
  (apply min (map second inputs)))

(defn bottom-bound [inputs]
  (apply max (map second inputs)))

(defn distance [x y coordinate]
  (+ (Math/abs (- x (first coordinate))) (Math/abs (- y (second coordinate)))))

(assert (= 0 (distance 1 1 [1 1])))
(assert (= 4 (distance 5 1 [1 1])))
(assert (= 4 (distance 5 1 [5 5])))

(defn closest-coordinate [x y coordinates]
  (let [coordinate->distances (zipmap coordinates (map #(distance x y %) coordinates))
        closest (apply min-key val coordinate->distances)
        min-distance (val closest)
        min-coordinates (filter #(= min-distance (val %)) coordinate->distances)
        count-min (count min-coordinates)]
    (if (= count-min 1) (first (first min-coordinates)) nil)))

(def test-inputs 
  '([1, 1]
    [1, 6]
    [8, 3]
    [3, 4]
    [5, 5]
    [8, 9]))

(assert (= [1 1] (closest-coordinate 0 1 test-inputs)))
(assert (= nil (closest-coordinate 5 1 test-inputs)))

(defn get-boundary [coordinates]
  (let [left (left-bound coordinates)
        top (top-bound coordinates)
        right (right-bound coordinates)
        bottom (bottom-bound coordinates)]
    (concat
      (map #(vector left %) (range top bottom))
      (map #(vector top %) (range left right))
      (map #(vector right %) (range top bottom))
      (map #(vector bottom %) (range left right)))))

(defn get-area [coordinates]
  (let [left (left-bound coordinates)
        top (top-bound coordinates)
        right (right-bound coordinates)
        bottom (bottom-bound coordinates)]
    (for [x (range left (inc right))
          y (range top (inc bottom))]
      [x y])))

(defn is-infinite [coordinate all-coordinates]
  (let [boundary (get-boundary all-coordinates)]
    (some #(= % coordinate)
          (map #(closest-coordinate (% 0) (% 1) all-coordinates) boundary))))

(assert (= true (is-infinite [1 1] test-inputs)))
(assert (= nil (is-infinite [5 5] test-inputs)))

(defn covered-area [coordinate point->closest]
  (count (filter #(= % coordinate) (vals point->closest))))

(defn most-covered-area [all-coordinates]
  (let [filtered-coordinates (filter #(not (is-infinite % all-coordinates)) all-coordinates)
        area (get-area all-coordinates)
        point->closest (zipmap area (map #(closest-coordinate (% 0) (% 1) all-coordinates) area))]
    (apply max-key val (zipmap filtered-coordinates (map #(covered-area % point->closest) filtered-coordinates)))))

(assert (= [[5 5] 17] (most-covered-area test-inputs)))

(defn parse-vector [input]
  (vec (for [x (str/split input #", ")] (read-string x))))

(def parsed-input
  (map parse-vector (str/split-lines (slurp "inputs/puzzle6"))))

(defn -main []
  (println (most-covered-area parsed-input)))
