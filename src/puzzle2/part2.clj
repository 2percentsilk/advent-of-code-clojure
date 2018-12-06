;; https://adventofcode.com/2018/day/2#part2
(ns puzzle2.part2
  (:require [clojure.string :as str])
  (:require [clojure.data :as data]))

(defn hash-positions [x]
  (into {} (map-indexed hash-map (str/split x #""))))

(assert (= {0 "a", 1 "b", 2 "c"} (hash-positions "abc")))

(defn overlap-string [map-input]
  (str/join (vals (into (sorted-map) map-input))))

(defn get-overlapping-part [x y]
  (let [hash-x (hash-positions x)
        hash-y (hash-positions y)
        expected-similarity (dec (count x))
        overlap-map (last (data/diff hash-x hash-y))]
    (if (= expected-similarity (count overlap-map)) (overlap-string overlap-map))
  ))

(assert (= "ab" (get-overlapping-part "abc" "abd")))
(assert (= "fgij" (get-overlapping-part "fghij" "fguij")))
(assert (= nil (get-overlapping-part "abcde" "axcye")))

(defn solve [input]
  (let [all-valid-overlaps (for [x1 input x2 input] (get-overlapping-part x1 x2))]
    (some identity all-valid-overlaps)))

(def test-input '("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"))

(assert (= "fgij" (solve test-input)))

(def parsed-input
  (str/split-lines (slurp "inputs/puzzle2")))

(defn -main []
  (println (solve parsed-input)))
