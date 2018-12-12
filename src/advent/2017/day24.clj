;; https://adventofcode.com/2017/day/24
(ns advent.2017.day24
  (:require [clojure.string :as str]))

(def test-input
  '("0/2"
    "2/2"
    "2/3"
    "3/4"
    "3/5"
    "0/1"
    "10/1"
    "9/10"))

(def puzzle-input (->> (slurp "inputs/2017/day24")
                       str/split-lines))

(defn parse-component [input]
  (vec (map read-string (str/split input #"/"))))

(def test-comps (map parse-component test-input))
(def puzzle-comps (map parse-component puzzle-input))

(defn potential-ends [available-components start-at]
  (->> available-components
       (filter (fn [[x y]] (or (= x start-at) (= y start-at))))
       (map (fn [[x y]] (if (= x start-at) y x)))
       seq))

(defn remaining [available-components [x y]]
  (filter (fn [[a b]] (not (or (and (= x a) (= y b)) (and (= x b) (= y a)))))
          available-components))

(defn strongest-bridge [available-components start-at]
  "returns strength of bridge"
  (if-let [potential (potential-ends available-components start-at)]
    (apply max (map #(+ % start-at (strongest-bridge (remaining available-components [% start-at]) %)) 
                    potential))
    0))

(defn longest-bridge-length [available-components start-at]
  "returns length of longest bridge"
  (if-let [potential (potential-ends available-components start-at)]
    (apply max (map #(+ 1 (longest-bridge-length (remaining available-components [% start-at]) %))
                    potential))
    0))

(defn strongest-bridge-of-length [available-components start-at remaining-length]
  "returns length of longest bridge of given length"
  (let [potential (potential-ends available-components start-at)]
    (if (= remaining-length 0) 0
        (if (nil? potential) nil
            (->> potential
                 (map #(vector % (strongest-bridge-of-length (remaining available-components [% start-at]) % (dec remaining-length))))
                 (filter (fn [[x y]] (some? y)) )
                 (map (fn [[x y]] (+ start-at x y)))
                 (concat '(0)) ; for max args
                 (apply max))))))

(assert (= 31 (strongest-bridge test-comps 0))
(assert (= 4 (longest-bridge-length test-comps 0))
(assert (= 19 (strongest-bridge-of-length test-comps 0 4)))

; Part 1
#_(strongest-bridge puzzle-comps 0)

; Part 2
#_(strongest-bridge-of-length puzzle-comps 0 
                            (longest-bridge-length puzzle-comps 0))