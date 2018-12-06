;; https://adventofcode.com/2018/day/6#part2
(ns puzzle6.part2
  (:use [puzzle6.part1 :exclude (-main)]))

(defn sum-of-distances [x y coordinates]
  (let [coordinate->distances (zipmap coordinates (map #(distance x y %) coordinates))]
    (reduce + (vals coordinate->distances))))

(assert (= 30 (sum-of-distances 4 3 test-inputs)))

(defn area-sum-of-distances [coordinates]
  (let [area (get-area coordinates)]
    (zipmap area (map #(sum-of-distances (% 0) (% 1) coordinates) area))))

(defn count-area [coordinates limit]
  (count (filter #(< % limit) (vals (area-sum-of-distances coordinates)))))

(assert (= 16 (count-area test-inputs 32)))

(defn -main []
  (println (count-area parsed-input 10000)))
