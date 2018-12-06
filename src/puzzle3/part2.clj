;; https://adventofcode.com/2018/day/3#part2
(ns puzzle3.part2
  (:use [puzzle3.part1 :exclude (-main)]))

(defn final-fabric-state [claims]
  (let [claim-widths (map claim-width claims)
        claim-heights (map claim-height claims)]
    (claimed-fabric-state
      ; too tired to debug why this requires an offset of 1
      (+ (apply max claim-widths) 1)
      (+ (apply max claim-heights) 1)
      claims)
  ))

(defn is-row-covered-once [row left right]
  (every? #{1} (subvec row left (inc right))))

(defn is-fabric-covered-once [fabric left top right bottom]
  (every?
    (fn [x] (is-row-covered-once x left right))
    (subvec fabric top (+ bottom 1))))

(assert (= true (is-fabric-covered-once [[0 0] [0 1]] 1 1 1 1)))
(assert (= false (is-fabric-covered-once [[0 0] [0 2]] 1 1 1 1)))
(assert (= true (is-fabric-covered-once [[1 1] [1 1]] 0 0 1 1)))

(defn is-valid-claim [claim fabric-state]
  (let [[claim-id left top right bottom] claim]
    (is-fabric-covered-once
      fabric-state left top right bottom)))

(defn valid-claim-id [claim-strings]
  (let [claims (map parse-claim claim-strings)
        fabric (final-fabric-state claims)
        is-valid (fn [x] (is-valid-claim x fabric))]
    (first (first (filter is-valid claims)))
  ))

(defn -main [] (println (valid-claim-id parsed-input)))
