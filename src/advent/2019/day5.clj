(ns advent.2019.day5
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "inputs/2019/day5"))

(defn parse-input [input]
  (map read-string (str/split input #",")))

(defn build-state [parsed]
  {:start 0
   :data (->> parsed
              (map-indexed hash-map)
              (into {}))
   :outputs '()})

(defn rev-digits [x] (if (= x 0) '()
                     (conj (rev-digits (quot x 10)) (mod x 10))))

(defn parse-parameter [parameter]
  "returns indices of immediate mode parameters"
  (->> parameter
       rev-digits
       (map-indexed (fn [idx itm] (if (= itm 1) idx nil)))
       (filter #(not= nil %))))

(parse-parameter (quot 1002 100))

(defn parse-ins [instruction]
  {:op (mod instruction 100)
   :immediates (parse-parameter (quot instruction 100))})

(defn immediate? [idxs idx] (some #(= % idx) idxs))

(immediate? (:immediates (parse-ins 1002)) 1)

(defn action [{start :start data :data :as state} ins fn]
  (let [immediate-idxs (:immediates (parse-ins ins))
        op-1 (if (immediate? immediate-idxs 0) (get data (+ 1 start))
                 (get data (get data (+ 1 start))))
        op-2 (if (immediate? immediate-idxs 1) (get data (+ 2 start))
                 (get data (get data (+ 2 start))))
        result (get data (+ 3 start))]
    (assoc state
           :start (+ start 4)
           :data (assoc data result (fn op-1 op-2)))))

(defn add-action [state ins] (action state ins +))

(defn mul-action [state ins] (action state ins *))

(defn run-once [{start :start data :data :as state}]
  (condp = (:op (parse-ins (get data start)))
    99 (assoc state :start nil)
    1 (add-action state (get data start))
    2 (mul-action state (get data start))
    3 (assoc state
             :start (+ start 2)
             :data (assoc data (get data (inc start)) 1)) ; use 1 as input
    4 (assoc state
             :start (+ start 2)
             :outputs (conj (:outputs state) (get data (get data (inc start)))))
    :else "error"))

(defn run-all [{start :start :as state}]
  (if (nil? start) state
      (run-all (run-once state))))

(-> "1002,4,3,4,33"
    parse-input
    build-state
    run-all)

(-> "3,0,4,0,99"
    parse-input
    build-state
    run-all)

; Part 1
; :outputs (5074395 0 0 0 0 0 0 0 0 3)
(def part-1
  (-> puzzle-input
      parse-input
      build-state
      run-all))
