(ns advent.2018.day20
  (:require [clojure.string :as str]))

(def test-input "^ENWWW(NEEE|SSE(EE|N))$")
(def test-input-2 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
(def test-input-3 "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
(def puzzle-input (slurp "inputs/2018/day20"))

(def left-bracket \()
(def right-bracket \))
(def pipe \|)

(defn letter? [c]
  (or (= c \W)
      (= c \E)
      (= c \N)
      (= c \S)))

(defn opposites? [c1 c2]
  (let [s (set (vector c1 c2))]
    (or (= s #{\N \S})
        (= s #{\E \W}))))

(defn max-count
  ([x] x)
  ([x y] (if (> (count x) (count y)) x y))
  ([x y & more]
   (reduce max-count (max-count x y) more)))

(defn all-room-lengths [all-pattern]
  (loop [pattern all-pattern
         all-rooms []
         moves-stack '()
         start-for-level {}
         opts-for-level {}
         nesting-level 0]
    (if-let [c (first pattern)]
      (cond
        (= c \^) (recur (rest pattern) all-rooms moves-stack start-for-level opts-for-level nesting-level)
        (= c \$) all-rooms

        (and (letter? c)
             (opposites? c (first moves-stack))) (recur (rest pattern) all-rooms
                                                        (rest moves-stack)
                                                        start-for-level opts-for-level nesting-level)

        (and (letter? c)
             (not (opposites? c (first moves-stack)))) (recur (rest pattern)
                                                              (conj all-rooms (conj moves-stack c))
                                                              (conj moves-stack c)
                                                              start-for-level opts-for-level nesting-level)

        (= c left-bracket) (recur (rest pattern) all-rooms moves-stack
                                  (assoc start-for-level (inc nesting-level) moves-stack)
                                  (assoc opts-for-level (inc nesting-level) [])
                                  (inc nesting-level))

        (= c pipe) (recur (rest pattern) all-rooms
                          (get start-for-level nesting-level) start-for-level
                          (update opts-for-level nesting-level #(conj % moves-stack))
                          nesting-level)

        (= c right-bracket) (let [all-opts (conj (get opts-for-level nesting-level) moves-stack)
                                  best-moves-stack (apply max-count all-opts)]
                              (recur (rest pattern) all-rooms best-moves-stack
                                     (dissoc start-for-level nesting-level)
                                     (dissoc opts-for-level nesting-level)
                                     (dec nesting-level)))))))

; Part 1
#_(->> puzzle-input
       all-room-lengths
       (map #(count %))
       (apply max))

; Part 2
#_(->> puzzle-input
       all-room-lengths
       (filter #(>= (count %) 1000))
       (map #(frequencies %))
       distinct
       count)
