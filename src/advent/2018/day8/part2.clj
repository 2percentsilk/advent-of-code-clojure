;; https://adventofcode.com/2018/day/8#part2
(ns advent.2018.day8.part2
  (:use [advent2018.puzzle8.part1 :exclude (-main)]))

(defn child-for-meta-value [children meta-value]
  (get children (dec meta-value)))

(defn get-value [tree]
  (let [children (tree :children)
        meta (tree :meta)]
    (if (empty? children)
      (reduce + meta)
      (reduce + (map get-value
                     (remove nil?
                             (map #(child-for-meta-value children %) meta)))))))

(defn -main []
  (println (get-value (parse-tree puzzle-input))))
