(ns advent.2018.day22
  (:require [clojure.set :as set])
  (:use clojure.data.priority-map)
  (:refer-clojure :exclude [subseq rsubseq]))

; Puzzle input
; depth: 9171
; target: 7,721
(def puzzle-depth 9171)
(def puzzle-target [7 721])

(declare m-geologic-index)
(declare m-erosion-level)

(defn geologic-index [[x y] depth target]
  (cond
    (= [x y] [0 0]) 0
    (= [x y] target) 0
    (= y 0) (* x 16807)
    (= x 0) (* y 48271)
    :else (* (m-erosion-level [(dec x) y] depth target)
             (m-erosion-level [x (dec y)] depth target))))

(defn erosion-level [[x y] depth target]
  (mod (+ depth (m-geologic-index [x y] depth target)) 20183))

(def m-erosion-level (memoize erosion-level))
(def m-geologic-index (memoize geologic-index))

(defn region-type [[x y] depth target]
  (condp = (mod (m-erosion-level [x y] depth target) 3)
    0 "rocky"
    1 "wet"
    2 "narrow"))

(defn risk-level [[start-x start-y] [target-x target-y] depth]
  (let [all-points (for [x0 (range start-x (inc target-x)) y0 (range start-y (inc target-y))]
                     (vector x0 y0))]
    (->> all-points
         (map #(m-erosion-level % depth [target-x target-y]))
         (map #(mod % 3))
         (reduce +))))

(defn valid-tools-for [[x y] depth target]
  (condp = (region-type [x y] depth target)
    "rocky" #{"torch" "climbing"}
    "wet" #{"climbing" "neither"}
    "narrow" #{"torch" "neither"}))

(defn is-valid-tool [tool [x y] depth target]
  (contains? (valid-tools-for [x y] depth target) tool))

(defn changed-tool [tool current-xy depth target]
  (let [ts (valid-tools-for current-xy depth target)]
    (first (set/difference ts (set (vector tool))))))

(defn neighbours [node visited depth target]
  (->> node
       vector
       (mapcat (fn [[x y t]] (vector (vector (dec x) y t)
                                     (vector (inc x) y t)
                                     (vector x (dec y) t)
                                     (vector x (inc y) t)
                                     (vector x y (changed-tool t [x y] depth target)))))
       (filter (fn [[x y t]] (and (>= x 0) (>= y 0))))
       (filter #(not (contains? visited %)))))

(defn cost [[x0 y0 t0] [x1 y1 t1] depth target]
  (cond
    ; changed in-place
    (= [x0 y0 t0] [x1 y1 t1]) 0
    (= [x0 y0] [x1 y1]) 7
    ; moved
    (not (is-valid-tool t1 [x1 y1] depth target)) 1000000
    (= t0 t1) 1))

(defn neighbours-with-costs [node start-distance visited depth target]
  (let [nbs (neighbours node visited depth target)]
    (->> nbs
         (map #(cost node % depth target))
         (map #(+ start-distance %))
         (zipmap nbs))))

(defn merge-unvisited [unvisited [node distance]]
  (let [existing (get unvisited node)]
    (if (or (nil? existing) (> existing distance)) (assoc unvisited node distance)
        unvisited)))

(defn time-taken [[target-x target-y] target-tool depth]
  (loop [visited {}
         unvisited (priority-map [0 0 "torch"] 0)]
    (if (contains? visited [target-x target-y target-tool])
      (get visited [target-x target-y target-tool])
      (let [[min-node min-distance] (first unvisited)
            nbs (neighbours-with-costs min-node min-distance visited depth [target-x target-y])]
        (recur (assoc visited min-node min-distance)
               (dissoc (reduce merge-unvisited unvisited nbs) min-node))))))

; Part 1
#_(risk-level [0 0] puzzle-target puzzle-depth)

; Part 2
#_(time-taken puzzle-target "torch" puzzle-depth)
