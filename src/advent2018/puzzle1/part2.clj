;; https://adventofcode.com/2018/day/1#part2
(ns advent2018.puzzle1.part2
  (:require [clojure.string :as str]))

(defn find-match [list, x]
  "iterate over list to match x"
  (first (filter (fn [y] (= y x)) list)))

(defn current-sum [sums-so-far]
  (if (last sums-so-far) (last sums-so-far) 0))

; tests
(assert (= 0 (current-sum [])))
(assert (= 2 (current-sum [1 2])))

(defn find-match-or-append [sums-so-far current-value]
  "return sum if match found, or append to sums-so-far"
  (let [new-value (+ (current-sum sums-so-far) current-value)]
    (if (find-match sums-so-far new-value)
      new-value
      (conj sums-so-far new-value))))

(defn solve [input]
  (loop [x input
         sums-so-far [0]]
    (let [current-value (first x)
          iteration-result (find-match-or-append sums-so-far current-value)]
      (if (vector? iteration-result)
        (recur (if (= (count x) 1) input (drop 1 x)) iteration-result)
        iteration-result))))

; tests
(assert (= 0 (solve '(1 -1))))
(assert (= 1 (solve '(1 2 -2 -1))))
(assert (= 10 (solve '(3, 3, 4, -2, -4))))
(assert (= 5 (solve '(-6, 3, 8, 5, -6))))
(assert (= 14 (solve '(7, 7, -2, -7, -4))))

(def parsed-input
  (map read-string (str/split-lines (slurp "inputs/puzzle1"))))

(defn -main []
  (println (solve parsed-input)))
