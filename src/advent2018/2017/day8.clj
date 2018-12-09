;; https://adventofcode.com/2017/day/8
(ns advent2018.2017.day8
  (:require [clojure.string :as str]))

(def test-input
  '("b inc 5 if a > 1"
     "a inc 1 if b < 5"
     "c dec -10 if a >= 1"
     "c inc -20 if c == 10"))

(defn parse-first [instructions]
  (let [split (str/split (first instructions) #" ")]
    {:target       (get split 0)
     :change-type  (get split 1)
     :change-value (read-string (get split 2))
     :check-reg    (get split 4)
     :check-type   (get split 5)
     :check-value  (read-string (get split 6))}))

(defn is-cond-true [values parsed]
  (let [existing (get values (parsed :check-reg) 0)
        expected (parsed :check-value)
        type (parsed :check-type)]
    (cond
      (= type "<") (< existing expected)
      (= type ">") (> existing expected)
      (= type ">=") (>= existing expected)
      (= type "<=") (<= existing expected)
      (= type "==") (= existing expected)
      (= type "!=") (not (= existing expected)))))

(is-cond-true {} {:check-reg "a" :check-value 0 :check-type "=="})

(defn get-new-value [result parsed]
  (let [existing (get result (parsed :target) 0)
        type (parsed :change-type)
        expected (parsed :change-value)]
    (cond
      (= type "inc") (+ expected existing)
      (= type "dec") (- existing expected))))

(get-new-value {} {:target "b" :change-type "dec" :change-value 2})

(defn process [instructions]
  (loop [remaining instructions
         highest-value nil
         result {}]
    (cond
      (empty? remaining) [result highest-value]
      :else (let [parsed (parse-first remaining)
                  updated (is-cond-true result parsed)
                  new-value (get-new-value result parsed)]
              (recur (rest remaining)

                     (if (and updated (or (nil? highest-value) (> new-value highest-value)))
                       new-value
                       highest-value)

                     (if updated
                       (assoc result (parsed :target) new-value)
                       result))))))

(def puzzle-input
  (str/split-lines (slurp "inputs/2017/puzzle8")))

; Part 1
(apply max-key val (get (process puzzle-input) 0))

; Part 2
(get (process puzzle-input) 1)
