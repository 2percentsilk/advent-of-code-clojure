;; https://adventofcode.com/2018/day/1#part2

(ns puzzle1_part2
  (:require [clojure.string :as str]))

(defn find-match [list, x]
  "iterate over list to match x"
  (first (filter (fn [y] (= y x)) list)))

(defn current-sum [sums-so-far]
  (if (last sums-so-far) (last sums-so-far) 0))

(defn find-match-or-append [sums-so-far current-value]
  "return sum if match found, or append to sums-so-far"
  (let [new-value (+ (current-sum sums-so-far) current-value)]
    (if (find-match sums-so-far new-value)
      new-value
      (conj sums-so-far new-value))
  ))

(defn ^:dynamic solve [input sums-so-far]
  (let [current-value (get input 0)
        new-input (conj (subvec input 1) current-value)
        iteration-result (find-match-or-append sums-so-far current-value)]
    (if (vector? iteration-result)
      (solve new-input iteration-result)
      iteration-result)))

; tests
(assert (= 0 (solve [1 -1] [0])))
(assert (= 1 (solve [1 2 -2 -1] [0])))
(assert (= 10 (solve [3, 3, 4, -2, -4] [0])))
(assert (= 5 (solve [-6, 3, 8, 5, -6] [0])))
(assert (= 14 (solve [7, 7, -2, -7, -4] [0])))

(def parsed-input
  (vec (for
      [x (str/split (slurp "inputs/puzzle1") #"\n")]
      (read-string x))))

(defn -main []
  (println (solve parsed-input [0])))
