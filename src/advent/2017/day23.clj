(ns advent.2017.day23
  (:require [clojure.string :as str]))

(def puzzle-input
  (str/split-lines (slurp "inputs/2017/day23")))

(defn parse-lines [input-lines]
  (zipmap (range 0 (count input-lines))
          (map #(str/split % #" ") input-lines)))

(def puzzle (parse-lines puzzle-input))

(defn value [registers name]
  (let [parsed (read-string name)]
    (if (int? parsed) parsed (get registers name 0))))

(defn run [instructions initial-registers limit]
  (loop [i 0
         current 0
         registers initial-registers
         mul 0]
    (let [current-ins (get instructions current)]
      (if (and (some? current-ins) (< i limit))
        (let [ins-name (first current-ins)
              name-1 (second current-ins)
              name-2 (get current-ins 2)
              value-1 (value registers name-1)
              value-2 (value registers name-2)]
          (if (= current 19) (println registers))
          (condp = ins-name
            "set" (recur (inc i) (inc current) (assoc registers name-1 value-2) mul)

            "sub" (recur (inc i) (inc current) (assoc registers name-1 (- value-1 value-2)) mul)

            "mul" (recur (inc i) (inc current) (assoc registers name-1 (* value-1 value-2)) (inc mul))

            "jnz" (if (= value-1 0) (recur (inc i) (inc current)  registers mul)
                      (recur (inc i) (+ current value-2) registers mul))))
        [mul i]))))

(defn next-ins [instructions ins-idx]
  (let [ins (instructions ins-idx)
        name (first ins)
        ins-val (read-string (get ins 2))]
    (if (= name "jnz") [(inc ins-idx) (+ ins-val ins-idx)]
        [(inc ins-idx)])))

(sort (zipmap (range 0 32) (map #(next-ins puzzle %) (range 0 32))))

; Part 1
; (Takes 54,224 iterations)
#_(run puzzle {} 60000)

; Part 2
; h is only touched in line 26