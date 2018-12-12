;; https://adventofcode.com/2018/day/12
(ns advent.2018.day12
  (:require [clojure.string :as str]))

(defn parse-initial-state [input]
  (zipmap (range 0 (.length input)) (str/split input #"")))

(defn parse-rules [input]
  (let [split (map #(str/split % #" => ") input)
        ks (map first split)
        vs (map second split)]
    (zipmap ks vs)))

(def test-initial (parse-initial-state "#..#.#..##......###...###"))

(def test-rules
  (parse-rules
    ["...## => #"
     "..#.. => #"
     ".#... => #"
     ".#.#. => #"
     ".#.## => #"
     ".##.. => #"
     ".#### => #"
     "#.#.# => #"
     "#.### => #"
     "##.#. => #"
     "##.## => #"
     "###.. => #"
     "###.# => #"
     "####. => #"
     ]))

(def puzzle-input
  (vec (str/split-lines (slurp "inputs/2018/day12"))))

(def puzzle-initial
  (parse-initial-state (subs (first puzzle-input) 15)))

(def puzzle-rules (parse-rules (subvec puzzle-input 2)))

(defn next-state-of [i current-state rules]
  (let [rule-key (str (get current-state (- i 2) ".")
                      (get current-state (- i 1) ".")
                      (get current-state i ".")
                      (get current-state (+ i 1) ".")
                      (get current-state (+ i 2) "."))]
    (get rules rule-key ".")))

(defn next-state [current-state rules]
  (let [ks (keys current-state)
        mink (apply min ks)
        maxk (apply max ks)
        ks-new (sort (concat (range mink (inc maxk))
                             [(- mink 2) (- mink 1) (+ maxk 1) (+ maxk 2)]))
        new-states (map #(next-state-of % current-state rules) ks-new)
        resultm (zipmap ks-new new-states)]
    (select-keys resultm (for [[k v] resultm :when (= v "#")] k))))

(defn iterate-states [limit initial rules]
  (loop [n limit
         state initial]
    (cond
      (= n 0) state
      :else (recur (dec n) (next-state state rules)))))

(defn sum-of-state [state]
  (reduce + (map first (filter (fn [[k v]] (= v "#")) state))))

; Part 1
(sum-of-state (iterate-states 20 puzzle-initial puzzle-rules))

; Part 2
; Observed that the sum increases by 72 on every 2
; iterations after the 2000th iteration
; Sum at 2000 = 71458
(loop [i 2000
       sum 71458]
  (cond
    (< i 50000000000) (recur (+ 2 i) (+ 72 sum))
    :else sum))
