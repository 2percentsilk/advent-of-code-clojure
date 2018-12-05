(ns puzzle1
  (:require [clojure.string :as str]))

(defn solve []
  (reduce + (for [x (str/split (slurp "puzzle-1-input") #"\n")
                  ] (read-string x))))

(defn -main []
  (println (solve)))
