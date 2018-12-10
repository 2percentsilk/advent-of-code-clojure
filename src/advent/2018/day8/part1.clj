;; https://adventofcode.com/2018/day/8
(ns advent.2018.day8.part1
  (:require [clojure.string :as str]))

(def puzzle-input
  (vec (map read-string (str/split (slurp "inputs/2018/day8") #" "))))

(defn leaf [input-vec start-idx]
  (let [input (subvec input-vec start-idx)
        nodes (first input)
        meta-length (second input)]
    (if (= nodes 0)
      {:children  []
       :meta      (subvec input 2 (+ 2 meta-length))
       :start-idx start-idx
       :end-idx   (+ 2 start-idx meta-length)}
      nil)))

(assert (= {:children [] :meta [10 11 12] :start-idx 0 :end-idx 5}
           (leaf [0 3 10 11 12] 0)))
(assert (= nil (leaf [1 1 0 1 9 2] 0)))

(defn tree [input-vec start end]
  (let [input (subvec input-vec start end)
        nodes (first input)
        meta-length (second input)
        first-leaf (leaf input-vec start)]
    (if (= 0 nodes)
      ; is a leaf, return
      first-leaf

      ; else, is not a leaf, go deeper
      (loop [remaining nodes
             children []
             children-end (+ start 2)]
        (if (= 0 remaining)
          {:children children
           :meta (subvec input-vec children-end (+ children-end meta-length))
           :start-idx start
           :end-idx (+ children-end meta-length)}
          (let [result (tree input-vec children-end end)]
            (recur (dec remaining)
                   (conj children result)
                   (result :end-idx))))))))

; root only tree
(assert (= {:children [] :meta [10 11 12] :start-idx 0 :end-idx 5}
           (tree [0 3 10 11 12] 0 5)))

; one child tree
;   A [1 1 2]
;    \
;     B [10 11 12]
(def i [1 3 0 3 10 11 12 1 1 2])
(tree i 0 (.length i))

(defn parse-tree [input]
  (tree input 0 (.length input)))

(defn sum-metadata [input-tree]
  (let [meta-sum (reduce + (input-tree :meta))]
  (if (empty? (input-tree :children))
    meta-sum
    (+ meta-sum
       (reduce + (map sum-metadata (input-tree :children)))))))

(defn -main []
  (println (sum-metadata (parse-tree puzzle-input))))
