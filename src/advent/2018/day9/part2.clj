;; https://adventofcode.com/2018/day/9#part2
(ns advent.2018.day9.part2
  (:use [advent2018.puzzle9.part1 :exclude (-main)]))

; Takes about ~60 secs
(defn -main []
  (println (answer puzzle-players (* 100 puzzle-marble))))
