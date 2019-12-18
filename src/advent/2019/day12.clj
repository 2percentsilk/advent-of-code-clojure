(ns advent.2019.day12
  (:require [clojure.string :as str]))

(def x
  [{:x 17 :y -9 :z 4}
   {:x 2 :y 2 :z -13}
   {:x -1 :y 5 :z -1}
   {:x 4 :y 7 :z -7}])

(def v
  [{:x 0 :y 0 :z 0}
   {:x 0 :y 0 :z 0}
   {:x 0 :y 0 :z 0}
   {:x 0 :y 0 :z 0}])

(def state (map (fn [x y] {:p x :v y}) x v))

(defn v-dim-offset [{p0 :p} state dim]
  (->> state
       (map (fn [{p :p}] (cond
                           (> (dim p) (dim p0)) 1
                           (< (dim p) (dim p0)) -1
                           :else 0)))
       (reduce +)))

(defn v-offset [state]
  (map (fn [x] {:x (v-dim-offset x state :x)
                :y (v-dim-offset x state :y)
                :z (v-dim-offset x state :z)}) state))

(defn apply-gravity [state]
  (map (fn [{v :v :as moon} y] (assoc moon :v (merge-with + v y)))
       state (v-offset state)))

(defn apply-velocity [state]
  (map (fn [{v :v p :p :as moon}] (assoc moon :p (merge-with + p v))) state))

(defn time-step [state] (apply-velocity (apply-gravity state)))

(defn abs [x] (if (< x 0) (- x) x))

(defn energy-moon [{p :p v :v}]
  (* (reduce + (map abs (vals p)))
     (reduce + (map abs (vals v)))))

(defn energy [state] (reduce + (map energy-moon state)))

(defn -main [] (println (energy (nth (iterate time-step state) 1000))))