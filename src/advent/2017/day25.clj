(ns advent.2017.day25
  (:require [clojure.string :as str]))

(import java.util.LinkedList)

(def test-states
  {:A {0 [1 1 :B]
       1 [0 -1 :B]}
   :B {0 [1 -1 :A]
       1 [1 1 :A]}})

(def puzzle-states
  {:A {0 [1 1 :B]
       1 [0 -1 :D]}
   :B {0 [1 1 :C]
       1 [0 1 :F]}
   :C {0 [1 -1 :C]
       1 [1 -1 :A]}
   :D {0 [0 -1 :E]
       1 [1 1 :A]}
   :E {0 [1 -1 :A]
       1 [0 1 :B]}
   :F {0 [0 1 :C]
       1 [0 1 :E]}})

(defn move-right [tape]
  "rotates to left, so write head is shifted to the right"
  (do
    (.addLast tape (.removeFirst tape))
    (if (= -1 (.peekFirst tape)) (.addFirst tape 0))))

(defn move-left [tape]
  (do
    (.addFirst tape (.removeLast tape))
    (if (= -1 (.peekFirst tape)) (do
                                   (.addLast tape (.removeFirst tape))
                                   (.addFirst tape 0)))))

(defn write-move [tape to-write direction]
  ; (tape is mutable)
  (do
    (.set tape 0 to-write)
    (if (= direction 1) (move-right tape) (move-left tape))))

(defn run [limit states]
  "-1 denotes edge of the tape"
  (let [tape (LinkedList. '(0 -1 -1))]
    (loop [[i current] [0 :A]]
      (if (< i limit) (let [value (.peek tape)
                            [w d n] (get (get states current) value)
                            _ (write-move tape w d)]
                        (recur [(inc i) n]))
          tape))))

(defn count-1 [tape]
  (loop [sum 0]
    (let [e (.peekFirst tape)]
      (cond
        (nil? e) sum
        (= 1 e) (do
                  (.removeFirst tape)
                  (recur (inc sum)))
        :else (do
                (.removeFirst tape)
                (recur sum))))))

; Part 1
#_(count-1 (run 12317297 puzzle-states))

; Part 2 (TODO)
; looks like I need to complete the rest of 2017 to solve this
