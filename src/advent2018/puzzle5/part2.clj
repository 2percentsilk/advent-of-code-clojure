;; https://adventofcode.com/2018/day/5#part2
(ns advent2018.puzzle5.part2
  (:use [advent2018.puzzle5.part1 :exclude (-main)]))

(def test-string "dabAcCaCBAcCcaDA")

(defn removed [s char-idx]
  (remove
    #(or (= % (char char-idx)) (= % (char (+ char-idx 32))))
    s))

(defn result [s]
  (let [char-ranges (range 65 (+ 65 26))]
    (apply min (map (comp count final-reacted)
                    (map #(removed s %) char-ranges)))))

(assert (= 4 (result test-string)))

(defn -main []
  (println (result loaded-string)))
