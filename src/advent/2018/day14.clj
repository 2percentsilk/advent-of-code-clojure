(ns advent.2018.day14
  (:require [clojure.string :as str]))

(defn sum-as-v [sum]
  (mapv read-string (str/split (str sum) #"")))

(defn update-scores [scores e0 e1]
  (let [sum (+ (scores e0) (scores e1))
        sumv (sum-as-v sum)]
    (into scores sumv)))

(defn update-idx [current scores]
  (let [value (scores current)]
    (mod (+ current (inc value)) (count scores))))

(defn run-scores [limit initial-scores e0-idx e1-idx]
  (loop [n 0 scores initial-scores
         e0 e0-idx e1 e1-idx]
    (if (> (count scores) limit)
      scores
      (let [updated (update-scores scores e0 e1)]
        (recur (inc n) updated (update-idx e0 updated) (update-idx e1 updated))))))

(defn get-last-of [scores to-find]
  (if (> (count scores) (count to-find))
    (subvec scores (- (count scores) (count to-find)))))

(defn has-sub-vector [scores to-find start-idx]
  ; sending in start-idx to reduce search space and speed up output
  (let [s (str/join (subvec scores start-idx))
        f (str/join to-find)
        i (.indexOf s f)]
    (if (= -1 i) nil (+ i start-idx))))

(defn run-scores-part-2 [to-find initial-scores e0-idx e1-idx]
  (loop [n 0
         scores initial-scores
         prev-length 2
         e0 e0-idx
         e1 e1-idx]
    (if-let [idx (has-sub-vector scores to-find (max 0 (- prev-length (count to-find))))]
      idx
      (let [updated (update-scores scores e0 e1)]
        (recur (inc n) updated (count scores)
               (update-idx e0 updated) (update-idx e1 updated))))))

; Part 1
; Input: 540561
#_(let [input 540561]
    (-> (run-scores (+ input 10) [3 7] 0 1)
        (subvec input (+ input 10))
        str/join))

; Part 2
; Takes ~30 seconds to complete
#_(let [input 540561]
    (-> (map (comp read-string str) (str input))
        vec
        (run-scores-part-2 [3 7] 0 1)))
