(ns advent.2019.day6
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(def test-input 
  '("COM)B"
    "B)C"
    "C)D"
    "D)E"
    "E)F"
    "B)G"
    "G)H"
    "D)I"
    "E)J"
    "J)K"
    "K)L"))

(def puzzle-input (str/split-lines (slurp "inputs/2019/day6")))

(defn adjacency-list [graph input]
  (if (empty? input) graph
      (let [[l r] (str/split (first input) #"\)")
            g' (adjacency-list graph (rest input))]
        (assoc g' l
               (conj (get g' l #{}) r)))))

(defn root [graph]
  (first (set/difference (set (keys graph))
                         (set (apply concat (vals graph))))))

(defn parent-of [v graph]
  (->> (keys graph)
       (filter #(contains? (get graph %) v))
       first))

(defn parent-count [v graph counter]
  (let [p (parent-of v graph)]
    (if (nil? p) -1 (get counter p))))

(defn traverse [graph counter queue]
  "bfs to build counter"
  (if (empty? queue) counter
      (traverse graph
                (assoc counter (first queue)
                       (inc (parent-count (first queue) graph counter)))
                (concat (rest queue) (get graph (first queue))))))

(traverse (adjacency-list {} test-input)
          {}
          (conj '() (root (adjacency-list {} test-input))))

(defn count-orbits [graph]
  (traverse graph {} (conj '() (root graph))))

#_(def part-1
    (->> puzzle-input
         (adjacency-list {})
         count-orbits
         vals
         (reduce +)))
