(ns advent.2019.day2
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "inputs/2019/day2"))

(defn parse-input [input]
  (map read-string (str/split input #",")))

(defn build-state [parsed]
  {:start 0 :data (->> parsed
                       (map-indexed hash-map)
                       (into {}))})

(defn action [{start :start data :data} fn]
  (let [op-1 (get data (+ 1 start))
        op-2 (get data (+ 2 start))
        result (get data (+ 3 start))]
    {:start (+ start 4)
     :data (assoc data result (fn (op-1 data) (op-2 data)))}))

(defn add-action [state] (action state +))

(defn mul-action [state] (action state *))

(defn run-once [{start :start data :data :as state}]
  (condp = (get data start)
    99 {:start nil, :data data}
    1 (add-action state)
    2 (mul-action state)
    :else "error"))

(defn run-all [{start :start :as state}]
  (if (nil? start) state
      (run-all (run-once state))))

(defn at-zero [state]
  (-> state
      (get :data)
      (get 0)))

(defn replace-noun-verb [state noun verb]
  (update-in state [:data] merge {1 noun 2 verb}))

(defn correct-state [state] (replace-noun-verb state 12 2))

; Part 1: 3706713
(def part-1
  (-> puzzle-input
      parse-input
      build-state
      correct-state
      run-all
      at-zero))

; Part 2: 8609
(defn get-value [input noun verb]
  (-> input
      parse-input
      build-state
      (replace-noun-verb noun verb)
      run-all
      at-zero))

(defn find-noun-verb [input expected-value]
  (loop [noun 0 
         verb 0
         value (get-value input noun verb)]
    (cond
      (= value expected-value) (+ (* 100 noun) verb)
      (< verb 100) (recur noun (+ 1 verb) (get-value input noun (+ 1 verb)))
      (< noun 100) (recur (+ 1 noun) 0 (get-value input (+ 1 noun) 0)))))

(def part-2
  (find-noun-verb puzzle-input 19690720))
