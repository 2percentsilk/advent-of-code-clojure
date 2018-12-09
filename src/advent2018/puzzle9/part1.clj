;; https://adventofcode.com/2018/day/9
(ns advent2018.puzzle9.part1
  (:require [clojure.string :as str]))

(defn initial-scores [num-players]
  (vec (repeat num-players 0)))

(defn is-23-multiple [n] (= (mod n 23) 0))

(defn insert-position [game-state current-marble-idx]
  (inc (mod (+ current-marble-idx 1) (.length game-state))))

(defn insert [game-state current-marble-idx new-marble]
  (let [position (insert-position game-state current-marble-idx)
        at-end (= position (.length game-state))]
    (if at-end [position (conj game-state new-marble)]
               [position (vec (concat (conj (subvec game-state 0 position) new-marble)
                                      (subvec game-state position)))])))

(assert (= [1 [0 1]] (insert [0] 0 1)))
(assert (= [1 [0 2 1]] (insert [0 1] 1 2)))

(defn remove-idx-vector [game-state remove-idx]
  [(get game-state remove-idx) (vec (concat (subvec game-state 0 remove-idx)
                                            (subvec game-state (inc remove-idx))))])

(defn game-state-23 [game-state current-marble-idx]
  "returns vector of value removed, new state, new current index"
  (let [remove-idx (mod (- current-marble-idx 7) (.length game-state))
        [value new-state] (remove-idx-vector game-state remove-idx)]
    [value new-state remove-idx]))

(defn update-scores [player-scores player-id new-score]
  (update player-scores player-id #(+ % new-score)))

(defn next-player-id [current player-scores]
  (mod (inc current) (.length player-scores)))

(defn final-scores [num-players last-marble]
  (loop [player-scores (initial-scores num-players)
         player-id 0
         game-state [0]
         current-marble-idx 0
         new-marble 1]
    (let [is-23 (is-23-multiple new-marble)
          is-ended (< last-marble new-marble)]
      (cond
        is-ended player-scores

        (not is-23) (let [[new-idx new-state] (insert game-state current-marble-idx new-marble)]
                      (recur player-scores
                             (next-player-id player-id player-scores)
                             new-state
                             new-idx
                             (inc new-marble)))

        :else (let [[score-inc new-state new-idx] (game-state-23 game-state current-marble-idx)]
                (recur (update-scores player-scores player-id (+ new-marble score-inc))
                       (next-player-id player-id player-scores)
                       new-state
                       new-idx
                       (inc new-marble)))))))

(defn answer [num-players last-marble]
  (apply max (final-scores num-players last-marble)))

(def puzzle-input
  "returns num players and max marble"
  (let [input (str/split (slurp "inputs/puzzle9") #" ")]
    [(read-string (get input 0)) (read-string (get input 6))]))

(defn -main [] (println (let [[players marble] puzzle-input]
                          (answer players marble))))
