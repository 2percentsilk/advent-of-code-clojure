(ns advent.2017.day19
  (:require [clojure.string :as str]))

(def test-input
  '("     |          "
    "     |  +--+    "
    "     A  |  C    "
    " F---|----E|--+ "
    "     |  |  |  D "
    "     +B-+  +--+ "))

(def puzzle-input (str/split-lines (slurp "inputs/2017/day19")))

(defn parse-line [input y]
  (->> (str/split input #"")
       (zipmap (map #(vector % y) (range 0 (.length input))))
       (filter (fn [[k v]] (not (= " " v))))
       (into {})))

(defn parse-input [input-lines]
  (->> input-lines
       (map-indexed #(parse-line %2 %1))
       (into {})))

(defn start-at [input]
  (->> input
       (filter (fn [[[x y] v]] (= y 0)))
       first))

(defn next-xy [xy dir]
  (mapv + xy dir))

(defn letter? [path]
  (<= 65 (int (.charAt path 0)) 90))

(defn update-letters [path letters]
  (if (letter? path) (conj letters path) letters))

(defn turn-left [[x y]]
  [(* y 1) (* x -1)])

(defn turn-right [[x y]]
  [(* y -1) (* x 1)])

(defn next-dir [xy paths dir]
  (let [new-xy (next-xy xy dir)
        path (get paths new-xy)]
    (cond
      (= "+" path) (if-let [straight-path (get paths (next-xy new-xy dir))]
                     dir ; straight path exists -> stick to same dir
                     (if (some? (get paths (next-xy new-xy (turn-left dir))))
                       (turn-left dir) (turn-right dir)))
      :else dir)))

(defn run [paths]
  (loop [xy (first (start-at paths))
         dir [0 1] ; down
         letters []
         steps 1]
    (if-let [new-path (get paths (next-xy xy dir))]
      (recur (next-xy xy dir) (next-dir xy paths dir) (update-letters new-path letters) (inc steps))
      [steps letters])))

; Part 1
#_(->> puzzle-input
       parse-input
       run
       second
       str/join)

; Part 2
#_(->> puzzle-input
       parse-input
       run
       first)
