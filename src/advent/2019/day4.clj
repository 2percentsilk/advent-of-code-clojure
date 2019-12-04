(ns advent.2019.day4
  (:require [clojure.string :as str]))

(defn digits [number]
  (map read-string (-> number
                       str
                       (str/split #""))))

(defn adjacent? [number]
  (->> number
       digits
       (partition 2 1)
       (some (fn [[x y]] (= x y)))))

(defn increasing-digits? [number]
  (->> number
       digits
       (partition 2 1)
       (every? (fn [[x y]] (>= y x)))))

; (increasing-digits? 223455)

(defn valid? [number]
  (and (>= number 100000)
       (<= number 999999)
       (increasing-digits? number)
       (adjacent? number)))

; Part 1: 1605
(def part-1
  (->> (range 193651 (inc 649729))
       (filter valid?)
       count))

(defn shift [number]
  (let [ds (rest (digits number)) sds (take 5 (digits number))]
    (loop [result '() ds-1 ds ds-2 sds]
      (cond
        (empty? ds-1) result
        :else (recur (conj result (- (first ds-1) (first ds-2)))
                     (rest ds-1) (rest ds-2))))))

(defn adjacent-part-2? [number]
  (let [shifted (shift number)]
    (loop [p nil c (first shifted) rem (rest shifted)]
      (cond
        (and (nil? p) (= c 0) (not= 0 (first rem))) true
        (and (not= 0 p) (= 0 c) (not= 0 (first rem))) true
        (and (empty? rem) (= 0 c) (not= 0 p)) true
        (empty? rem) false
        :else (recur c (first rem) (rest rem))))))

; (adjacent-part-2? 111122)

(defn valid-part-2? [number]
  (and (>= number 100000)
       (<= number 999999)
       (increasing-digits? number)
       (adjacent-part-2? number)))

; Part 2: 1102
(def part-2
  (->> (range 193651 (inc 649729))
       (filter valid-part-2?)
       count))
