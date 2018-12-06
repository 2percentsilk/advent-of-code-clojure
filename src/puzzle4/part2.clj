;; https://adventofcode.com/2018/day/4#part2
(ns puzzle4.part2
  (:use [puzzle4.part1 :exclude (-main)]))

(defn max-minute-for-guards [guard-ranges]
  (let [id-keys (keys guard-ranges)]
    (map (fn [x] (conj (max-frequency-minute
                          (apply concat (vals (get guard-ranges x)))) x)) id-keys)))

; minute frequency guard-id
(assert (= '([24 2 10] [45 3 99]) (max-minute-for-guards (sleep-ranges-for-guards test-lines-2))))

(defn get-mid [result]
  (get result 1))

(defn max-mid [previous current]
  (let [previous-mid (get-mid previous)
        current-mid (get-mid current)]
    (if (> previous-mid current-mid) previous current)))

(defn get-best-value [guard-results]
  (let [best-value (reduce max-mid guard-results)]
    (* (get best-value 0) (get best-value 2))))

(assert (= 4455 (get-best-value '([24 2 10] [45 3 99]))))

(defn -main [] (println (get-best-value (max-minute-for-guards (sleep-ranges-for-guards parsed-input)))))
