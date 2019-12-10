(ns advent.2019.day5
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "inputs/2019/day5"))

(defn parse-input [input]
  (map read-string (str/split input #",")))

(defn build-state [parsed inputs]
  {:start 0
   :data (->> parsed
              (map-indexed hash-map)
              (into {}))
   :outputs '()
   :inputs inputs
   :rel-base 0})

(defn getter [data i] (get data i 0))

(defn rev-digits [x] (if (= x 0) '()
                         (conj (rev-digits (quot x 10)) (mod x 10))))

(defn parse-non-position [parameter mode]
  (->> parameter
       rev-digits
       (map-indexed (fn [idx itm] (if (= itm mode) idx nil)))
       (filter #(not= nil %))))

(defn parse-immediates [parameter]
  "returns indices of immediate mode parameters"
  (parse-non-position parameter 1))

(defn parse-relatives [parameter]
  "returns indices of relative mode parameters"
  (parse-non-position parameter 2))

(defn parse-ins [instruction]
  {:op (mod instruction 100)
   :immediates (parse-immediates (quot instruction 100))
   :relatives (parse-relatives (quot instruction 100))})

(defn idx-match? [idxs idx] (some #(= % idx) idxs))

(defn pos-i [{start :start data :data rel-base :rel-base} ins para-idx]
  (let [immediate-idxs (:immediates (parse-ins ins))
        relative-idxs (:relatives (parse-ins ins))]
    (cond
      (idx-match? immediate-idxs (dec para-idx)) (+ para-idx start)
      (idx-match? relative-idxs (dec para-idx)) (+ rel-base
                                                   (getter data (+ para-idx start)))
      :else (getter data (+ para-idx start)))))

(defn para-i [{data :data :as state} ins para-idx] (getter data (pos-i state ins para-idx)))

(defn arith-action [{start :start data :data :as state} ins fn]
  (let [para-1 (para-i state ins 1)
        para-2 (para-i state ins 2)
        pos (pos-i state ins 3)]
    (assoc state :start (+ start 4)
           :data (assoc data pos (fn para-1 para-2)))))

(defn add-action [state ins] (arith-action state ins +))

(defn mul-action [state ins] (arith-action state ins *))

(defn jump-action [{start :start :as state} ins fn]
  (assoc state
         :start (if (fn 0 (para-i state ins 1))
                  (para-i state ins 2)
                  (+ 3 start))))

(defn jump-if-true [state ins] (jump-action state ins not=))

(defn jump-if-false [state ins] (jump-action state ins =))

(defn compare-action [{start :start data :data :as state} ins fn]
  (assoc state :start (+ 4 start)
         :data (assoc data (pos-i state ins 3)
                      (if (fn (para-i state ins 1) (para-i state ins 2)) 1 0))))

(defn action-lt [state ins] (compare-action state ins <))

(defn action-eq [state ins] (compare-action state ins =))

(defn run-once [{start :start data :data inputs :inputs rel-base :rel-base
                 :as state}]
  (condp = (:op (parse-ins (getter data start)))
    99 (assoc state :start nil)
    1 (add-action state (getter data start))
    2 (mul-action state (getter data start))
    3 (assoc state
             :start (+ 2 start)
             :data (assoc data (pos-i state (getter data start) 1)
                          (peek inputs))
             :inputs (pop inputs))
    4 (assoc state
             :start (+ 2 start)
             :outputs (conj (:outputs state) 
                            (para-i state (getter data start) 1)))
    5 (jump-if-true state (getter data start))
    6 (jump-if-false state (getter data start))
    7 (action-lt state (getter data start))
    8 (action-eq state (getter data start))
    9 (assoc state 
             :start (+ 2 start)
             :rel-base (+ rel-base (para-i state (getter data start) 1)))
    :else "error"))

(defn run-all [{start :start :as state}]
  (if (nil? start) state
      (run-all (run-once state))))

; Part 1
; :outputs (5074395 0 0 0 0 0 0 0 0 0)
#_(def part-1
  (-> puzzle-input
      parse-input
      (build-state '(1))
      run-all
      :outputs))

; Part 2: 8346937
#_(def part-2
  (-> puzzle-input
      parse-input
      (build-state '(5))
      run-all
      :outputs))
