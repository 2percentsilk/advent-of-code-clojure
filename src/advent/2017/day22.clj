;; https://adventofcode.com/2017/day/22
(ns advent.2017.day22
  (:require [clojure.string :as str]))

(def test-input
  '("..#"
     "#.."
     "..."))

(defn input-row-map [row y-id]
  (into {} (map-indexed #(assoc {} [%1 y-id] %2) row)))

(defn input-map [input]
  (reduce into {} (map-indexed #(input-row-map %2 %1) input)))

(defn turn-right [[x y]]
  [(* -1 y) (* 1 x)])

(defn turn-left [[x y]]
  [(* 1 y) (* -1 x)])

(defn reverse-d [[x y]]
  [(* -1 x) (* -1 y)])

(defn update-map [input-map position direction bursts]
  [(update input-map position #(if (= \# %) \. \#))
   (map + position direction)
   (if (= \. (get input-map position \.)) (inc bursts) bursts)])

(defn update-map-part-2 [input-map position direction bursts]
  [(update input-map position #(cond
                                 (= % \#) \F
                                 (= % \W) \#
                                 (= % \F) \.
                                 :else \W))
   (map + position direction)
   (if (= \W (get input-map position \.)) (inc bursts) bursts)])

(defn run-bursts [input-map pos dir n update-map-fn]
  (loop [m input-map
         p pos
         d dir
         i n
         bursts 0]
    (let [current (get m p \.)
          dnew (condp = current
                 \. (turn-left d)
                 \W d
                 \# (turn-right d)
                 \F (reverse-d d))
          [mnew pnew bnew] (update-map-fn m p dnew bursts)]
      (cond
        (= i 0) bursts
        :else (recur mnew pnew dnew (dec i) bnew)
        ))))

(def puzzle-input
  (str/split-lines (slurp "inputs/2017/day22")))

(defn get-mid [puzzle-map]
  (let [ks (keys puzzle-map)
        max-x (apply max (map first ks))
        max-y (apply max (map second ks))]
    [(/ max-x 2) (/ max-y 2)]))

; Part 1
#_(run-bursts (input-map puzzle-input)
              (get-mid (input-map puzzle-input))
              [0 -1] 10000
              update-map)

; Part 2
; Elapsed time: 42025.886953 msecs
#_(run-bursts (input-map puzzle-input)
              (get-mid (input-map puzzle-input))
              [0 -1] 10000000
              update-map-part-2)
