(ns advent.2019.day1
  (:require [clojure.string :as str]))

(defn mass [x]
  (- (int (Math/floor (/ x 3))) 2))

(def parsed-input
  (map read-string (str/split-lines (slurp "inputs/2019/day1"))))

(defn fuel [modules] (reduce + (map mass modules)))

(defn all-fuel [module]
  (loop [start (mass module)
         all start]
    (if (> (mass start) 0)
      (recur (mass start) (+ all (mass start)))
      all)))

(defn -main []
  (println (reduce + (map all-fuel parsed-input))))
