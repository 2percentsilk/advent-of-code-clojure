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

; (immediate? (:immediates (parse-ins 1002)) 1)

(defn para-i  [{start :start data :data} ins para-idx]
  (let [immediate-idxs (:immediates (parse-ins ins))]
    (if (immediate? immediate-idxs (dec para-idx)) (get data (+ para-idx start))
        (get data (get data (+ para-idx start))))))

(defn action [{start :start data :data :as state} ins fn]
  (let [para-1 (para-i state ins 1)
        para-2 (para-i state ins 2)
        para-3 (get data (+ 3 start))]
    (assoc state :start (+ start 4)
           :data (assoc data para-3 (fn para-1 para-2)))))

(defn add-action [state ins] (action state ins +))

(defn mul-action [state ins] (action state ins *))

(defn run-once [{start :start data :data :as state} input]
  (condp = (:op (parse-ins (get data start)))
    99 (assoc state :start nil)
    1 (add-action state (get data start))
    2 (mul-action state (get data start))
    3 (assoc state
             :start (+ 2 start)
             :data (assoc data (get data (inc start)) input)) ; use 1 as input
    4 (assoc state
             :start (+ 2 start)
             :outputs (conj (:outputs state) (para-i state (get data start) 1)))
    5 (assoc state 
             :start (if (not= 0 (para-i state (get data start) 1))
                      (para-i state (get data start) 2)
                      (+ 3 start)))
    6 (assoc state
             :start (if (= 0 (para-i state (get data start) 1) )
                      (para-i state (get data start) 2)
                      (+ 3 start)))
    7 (assoc state
             :start (+ 4 start)
             :data (assoc data (get data (+ 3 start))
                          (if (< (para-i state (get data start) 1)
                                 (para-i state (get data start) 2))
                            1 0)))
    8 (assoc state
             :start (+ 4 start)
             :data (assoc data (get data (+ 3 start))
                          (if (= (para-i state (get data start) 1)
                                 (para-i state (get data start) 2))
                            1 0)))
    :else "error"))

(defn run-all [{start :start :as state} input]
  (if (nil? start) state
      (run-all (run-once state input) input)))

; (-> "1002,4,3,4,33"
;     parse-input
;     build-state
;     run-all)

; (-> "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
;     parse-input
;     build-state
;     (run-all 8))

; Part 1
; :outputs (5074395 0 0 0 0 0 0 0 0 0)
(def part-1
  (-> puzzle-input
      parse-input
      build-state
      (run-all 1)))

; Part 2: 8346937
(def part-2
  (-> puzzle-input
      parse-input
      build-state
      (run-all 5)))
