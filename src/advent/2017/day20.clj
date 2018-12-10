;; https://adventofcode.com/2017/day/20
(ns advent.2017.day20
  (:require [clojure.string :as str]))

(defn v [t u a]
  (+ u (* t a)))

(defn s [t p0 u a]
  "coordinate at time t"
  (+ p0 (* u t) (* a (/ (* t (inc t)) 2))))

(def test-input
  '("p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>"
     "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"))

(def puzzle-input
  (str/split-lines (slurp "inputs/2017/day20")))

(defn parse-coordinates [input]
  (vec (map read-string (str/split input #","))))

(defn parse [input]
  (vec (map parse-coordinates
            (rest (re-find #"p=<(.+)>, v=<(.+)>, a=<(.+)>" input)))))

(defn d [position]
  (reduce + (map #(Math/abs %) position)))

(defn closest-acc [parsed]
  (let [accs (map #(get % 2) parsed)
        acc-ds (vec (map d accs))
        acc-dm (zipmap (range 0 (count accs)) acc-ds )]
    (apply min-key val acc-dm)))

(defn updated [point]
  (let [p (get point 0)
        v (get point 1)
        a (get point 2)
        updated-v (vec (map-indexed #(+ %2 (get a %1)) v))]
  [; positions
   (vec (map-indexed #(+ %2 (get updated-v %1)) p))
   ; velocity
   updated-v
   ; acceleration is constant
   a]))

(defn remove-collided [points]
  (let [positions (map #(get % 0) points)
        f (frequencies positions)
        common (set (map #(% 0) (filter #(> (% 1) 1) f)))]
    (filter #(not (contains? common (% 0))) points)))

(defn collision-course [inputs limit]
  (loop [points inputs
         n 0]
    (println n)
    (cond
      (> n limit) points
      (<= (count points) 1) points
      :else (recur (remove-collided (map updated points)) (inc n)))))

(def test-inputs
  '("p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>"
     "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>"
     "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>"
     "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"))

; Part 1
; Logic: long term position will be dominated by acceleration values
; hence we search for the "closest acceleration"
; This returns [id, acceleration distance]
(closest-acc (map parse puzzle-input))

; Part 2
; Simulate collision course till limit
; There is probably a better way to detect that no collisions can
; happen
(count (collision-course (map parse puzzle-input) 10000))
