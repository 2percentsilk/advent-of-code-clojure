(ns advent.2019.day16
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "inputs/2019/day16"))

(defn repeated [base pos] (mapcat #(repeat pos %) base))

(defn pattern [length pos]
  (rest (take (inc length) (cycle (repeated '(0 1 0 -1) pos)))))

(defn mul [xs ys] (reduce + (map * xs ys)))

(defn units [n] (if (< n 0) (mod (- n) 10) (mod n 10)))

(defn phase-output [inputs]
  (let [len (count inputs)]
    (->> inputs
         (repeat len)
         (map-indexed (fn [idx itm] (mul (pattern len (inc idx)) itm)))
         (map units))))

(defn n-phases [n input]
  (nth (iterate phase-output input) n))

; Part 1: 37153056
#_(time 
   (->> (str/split puzzle-input #"")
        (map read-string)
        (n-phases 100)
        (take 8)
        (apply str))
   )

#_(time
   (->> (str/split puzzle-input #"")
        (map read-string)
        (mapcat #(repeat 10000 %))
        (n-phases 100)
        (take 8)))
