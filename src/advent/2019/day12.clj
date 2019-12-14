(ns advent.2019.day12
  (:require [clojure.string :as str])
  (:require [clojure.math.numeric-tower :as math]))

(def test-pos
  '("<x=-1, y=0, z=2>"
    "<x=2, y=-10, z=-7>"
    "<x=4, y=-8, z=8>"
    "<x=3, y=5, z=-1>"))

(def x
  [{:x -1 :y 0 :z 2}
   {:x 2 :y -10 :z -7}
   {:x 4 :y -8 :z 8}
   {:x 3 :y 5 :z -1}])

(def v
  [{:x 0 :y 0 :z 0}
   {:x 0 :y 0 :z 0}
   {:x 0 :y 0 :z 0}
   {:x 0 :y 0 :z 0}])

(def state (map (fn [x y] {:p x :v y}) x v))

(defn v-offset [{p1 :p v1 :v} state]
  (map #({:x (v-dim-offset (:x p1) ())}) state))

(apply v-offset (first (partition 2 1 state)))

(defn apply-gravity [x v]
  )

(defn time-step [x v])