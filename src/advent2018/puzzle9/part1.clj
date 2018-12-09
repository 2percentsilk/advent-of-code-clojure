;; https://adventofcode.com/2018/day/9
(ns advent2018.puzzle9.part1
  (:require [clojure.string :as str]))

; importing Doubly Linked List from java lib
; https://docs.oracle.com/javase/8/docs/api/index.html?java/util/LinkedList.html
(import java.util.LinkedList)

(defn initial-scores [num-players]
  (vec (repeat num-players 0)))

(defn is-23-multiple [n] (zero? (mod n 23)))

(defn update-scores [player-scores player-id new-score]
  (update player-scores player-id #(+ % new-score)))

(defn next-player-id [current player-scores]
  (mod (inc current) (.length player-scores)))

(defn rotate-left [game-state num]
  ; game-state is mutable linked list
  (dotimes [n num]
    (.addLast game-state (.removeFirst game-state))))

(defn rotate-right [game-state num]
  ; game-state is mutable linked list
  (dotimes [n num]
    (.addFirst game-state (.removeLast game-state))))

(defn final-scores [num-players last-marble]
  (let [game-state (LinkedList. '(0))]
    ; mutable local scope state --> modified by helpers
    (loop [player-scores (initial-scores num-players)
           player-id 0
           new-marble 1]
      (let [is-23 (is-23-multiple new-marble)
            is-ended (< last-marble new-marble)]
        (cond
          is-ended player-scores

          is-23 (let [_ (rotate-right game-state 7)
                      value (.removeLast game-state)
                      _ (rotate-left game-state 1)]
                  (recur (update-scores player-scores player-id (+ new-marble value))
                         (next-player-id player-id player-scores)
                         (inc new-marble)))

          :else (let [_ (rotate-left game-state 1)
                      _ (.addLast game-state new-marble)]
                  (recur player-scores
                         (next-player-id player-id player-scores)
                         (inc new-marble))))))))

(defn answer [num-players last-marble]
  (apply max (final-scores num-players last-marble)))

(def puzzle-input
  "returns num players and max marble"
  (let [input (str/split (slurp "inputs/puzzle9") #" ")]
    [(read-string (get input 0)) (read-string (get input 6))]))

(def puzzle-players (get puzzle-input 0))
(def puzzle-marble (get puzzle-input 1))

; Takes about ~0.6 secs
(defn -main []
  (println (answer puzzle-players puzzle-marble)))
