(ns advent.2017.day3
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.math.numeric-tower :as math]))

(def puzzle-input 347991)
(defn sqr [x] (math/expt x 2))
(defn abs [x] (max x (- x)))
(defn square? [x] (zero? (mod (Math/sqrt x) 1)))

(defn odd-sq-ceil [x]
  (if (and (square? x) (odd? x)) x
      (odd-sq-ceil (inc x))))

(defn ring [number]
  (/ (- (int (Math/sqrt (odd-sq-ceil number))) 1) 2))

(defn ring-max [ring] (sqr (inc (* 2 ring))))

(defn ring-centers [ring]
  (let [rmax (ring-max ring)]
    [(- rmax ring)
     (- rmax (* 3 ring))
     (- rmax (* 5 ring))
     (- rmax (* 7 ring))]))

(defn distance-from-ring-center [number] 
  (->> (ring-centers (ring number))
       (map #(abs (- % number)))
       (apply min)))

(defn distance [number]
  (+ (ring number) (distance-from-ring-center number)))

; Part 1: 480
(def part-1 (distance puzzle-input))

(defn next [[x y] r]
  (cond
    (and (= y (- r)) (>= x (- r)) (<= x r)) [(inc x) y]
    (and (= x r) (< y r) (> y (- r))) [x (inc y)]
    (and (= y r) (<= x r) (> x (- r))) [(dec x) y]
    (and (= x (- r)) (<= y r) (> y (- r))) [x (dec y)]))

(defn ring-update [[x y] r]
  (if (and (= y (- r)) (= x r)) (inc r) r))

(defn neighbors [[x y]]
  (let [xs (map + (range -1 2) (repeat x))
        ys (map + (range -1 2) (repeat y))]
    (->> (combo/cartesian-product xs ys)
         (map #(apply vector %))
         (filter #(not= % [x y])))))

(defn next-val [state pos]
  (->> pos
       neighbors
       (map #(get state % 0))
       (reduce +)))

(defn fill [limit]
  (loop [state {[0 0] 1} ring 1 pos [1 0]]
    (let [val (next-val state pos)]
      (if (> val limit) val
          (recur (assoc state pos val) (ring-update pos ring) (next pos ring))))))

; Part 2: 349975
(def part-2 (fill puzzle-input))
