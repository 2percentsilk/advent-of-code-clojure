(ns advent.2019.day8
  (:require [clojure.string :as str]))

(def test-input "0222112222120000")
(def puzzle-input (slurp "inputs/2019/day8"))

(defn parse [input] (map read-string (str/split input #"")))

(defn layers [input w h] (partition (* w h) input))

(defn count-n [layer n] (count (filter #(= n %) layer)))

(defn count-0 [layer] (count-n layer 0))

(defn fewest-0-layer [layers] (apply min-key count-0 layers))

(defn part-1 [input w h]
  (let [selected (-> input parse (layers w h) fewest-0-layer)]
    (* (count-n selected 1) (count-n selected 2))))

; Part 1: 2125
#_(part-1 puzzle-input 25 6)

(defn first-opaque [pixels] (some #{0 1} pixels))

(defn visible [layers] (apply map (fn [& ps] (first-opaque ps)) layers))

(defn message [layer w h]
  (loop [x 0 rem layer res ""]
    (cond
      (empty? rem) res
      (= x w) (recur 0
                     rem 
                     (str res \newline))
      :else (recur (inc x) 
                   (rest rem) 
                   (str res (if (= 0 (first rem)) \space \*))))))

; Part 2: JYZHF
#_ (def part-2
     (-> puzzle-input
         parse
         (layers 25 6)
         visible
         (message 25 6)
         println))
