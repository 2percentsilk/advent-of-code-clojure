(ns advent.2017.day17)

(import java.util.LinkedList)

(defn jump-tape [tape jumps]
  (if (empty? tape) nil
      (dotimes [n jumps]
        (.addLast tape (.removeFirst tape)))))

(defn insert [tape value]
  (do
    (jump-tape tape 1)
    (.addFirst tape value)))

(defn run-insertions [limit jumps]
  (let [tape (LinkedList. '())]
    (loop [n 0]
      (if (> n limit)
        (do
          (jump-tape tape 1)
          (.peekFirst tape))
        (do
          (jump-tape tape jumps)
          (if (= 0 (.peekFirst tape)) (println "inserting" n))
          (insert tape n)
          (recur (inc n)))))))

(defn is-after-0 [current-idx size jumps]
  (= 0 (mod (+ current-idx jumps) size)))

(defn inserted-after-0 [limit jumps]
  (loop [n 1
         current-idx 0
         after-0 nil]
    (if (> n limit) after-0
        (if (is-after-0 current-idx n jumps)
          (recur (inc n) 1 n)
          (recur (inc n) (inc (mod (+ current-idx jumps) n)) after-0)))))

; Part 1
; Input is 335
#_(run-insertions 2017 335)

; Part 2
#_(inserted-after-0 50000000 335)
