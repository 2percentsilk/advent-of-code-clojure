;; https://adventofcode.com/2017/day/21
(ns advent.2017.day21
  (:require [clojure.string :as str]))

(def puzzle-input
  (->> (slurp "inputs/2017/day21")
       str/split-lines))

(def test-input
  '("../.# => ##./#../..."
     ".#./..#/### => #..#/..../..../#..#"))

(defn get-rule-book [input]
  "returns hash map of rules"
  (let [split (map #(str/split % #" => ") input)]
    (zipmap (map first split) (map second split))))

(def initial
  [".#."
   "..#"
   "###"])

(defn rotate-right [input]
  (vec (map #(str/join "" %) (apply mapv #(into [] %&)
                                    (reverse input)))))

(defn flip-v [input]
  (vec (map str/reverse input)))

(defn flip-h [input]
  (vec (reverse input)))

(defn make-key [input]
  (str/join "/" input))

(defn rotation-keys [input]
  (loop [i input
         n 4
         keys []]
    (if (> n 0) (recur (rotate-right i) (dec n) (conj keys (make-key i)))
                keys)))

(defn possible-keys [input]
  "we can rotate or flip"
  (let [fv (flip-v input)
        fh (flip-h input)]
    (concat (rotation-keys input)
            (rotation-keys fv)
            (rotation-keys fh))))

(defn transformation [input rule-book]
  (str/split (second (first (select-keys rule-book (possible-keys input)))) #"/"))

(defn break-rows [rows size]
  "number of rows is equal to size"
  (let [row-1 (first rows)]
    (if (empty? row-1) []
                       (conj (break-rows (vec (map #(subs % size) rows))
                                         size)
                             (map #(subs % 0 size) rows)))))

(defn break-into [input size]
  (loop [result []
         row-start 0]
    (if (< row-start (count input))

      (recur (concat result (vec (reverse (break-rows (subvec input row-start (+ size row-start)) size))))
             (+ row-start size))

      result)))

(defn merged-row [input]
  (loop [i 0
         row []]
    (if (< i (count (first input)))

      (recur (inc i)
             (conj row (str/join #"" (reduce concat (map #(% i) input)))))

      row)))


(defn merged [t num-in-row]
  (if (empty? t) []
                 (vec (concat (merged-row (subvec t 0 num-in-row))
                              (merged (subvec t num-in-row) num-in-row)))))

(defn next-state [input rule-book]
  (let [input-size (count input)
        broken-size (if (zero? (mod input-size 2)) 2 3)
        num-in-row (/ input-size broken-size)
        broken (vec (map vec (break-into input broken-size)))
        t (vec (map #(transformation % rule-book) broken))]
    (merged t num-in-row)))

(defn gen-art [rule-book limit]
  (loop [n 0
         s initial]
    (if (< n limit) (recur (inc n)
                           (next-state s rule-book))
                    s)))

(defn count-pixels [input]
  (count (filter #(= "#" %) (flatten (map #(str/split % #"") input)))))

; Part 1
(count-pixels (gen-art (get-rule-book puzzle-input) 5))

; Part 2 (TODO)
; Implementation is not good enough to solve part 2
; "Exception in thread "main" java.lang.OutOfMemoryError: GC overhead limit exceeded"
(defn -main []
  (println (count-pixels (gen-art (get-rule-book puzzle-input) 18))))
