(ns advent.2018.day21
  (:require [clojure.string :as str]))

(def puzzle-input (str/split-lines (slurp "inputs/2018/day21")))

(defn addr [regs [A B C]]
  (assoc regs C (+ (get regs A) (get regs B))))

(defn addi [regs [A B C]]
  (assoc regs C (+ (get regs A) B)))

(defn mulr [regs [A B C]]
  (assoc regs C (* (get regs A) (get regs B))))

(defn muli [regs [A B C]]
  (assoc regs C (* (get regs A) B)))

(defn banr [regs [A B C]]
  (assoc regs C (bit-and (get regs A) (get regs B))))

(defn bani [regs [A B C]]
  (assoc regs C (bit-and (get regs A) B)))

(defn borr [regs [A B C]]
  (assoc regs C (bit-or (get regs A) (get regs B))))

(defn bori [regs [A B C]]
  (assoc regs C (bit-or (get regs A) B)))

(defn gtir [regs [A B C]]
  (if (> A (get regs B))
    (assoc regs C 1)
    (assoc regs C 0)))

(defn gtri [regs [A B C]]
  (if (> (get regs A) B)
    (assoc regs C 1)
    (assoc regs C 0)))

(defn gtrr [regs [A B C]]
  (if (> (get regs A) (get regs B))
    (assoc regs C 1)
    (assoc regs C 0)))

(defn eqir [regs [A B C]]
  (if (= A (get regs B))
    (assoc regs C 1)
    (assoc regs C 0)))

(defn eqri [regs [A B C]]
  (if (= (get regs A) B)
    (assoc regs C 1)
    (assoc regs C 0)))

(defn eqrr [regs [A B C]]
  (if (= (get regs A) (get regs B))
    (assoc regs C 1)
    (assoc regs C 0)))

(defn setr [regs [A B C]]
  (assoc regs C (get regs A)))

(defn seti [regs [A B C]]
  (assoc regs C A))

(defn call [name & args]
  (when-let [fun (ns-resolve 'advent.2018.day21 (symbol name))]
    (apply fun args)))

(defn regs-state [instruction-lines start-regs ip-reg limit]
  (loop [regs start-regs
         current-ins 0
         i 0]
    (if (> i limit) [i regs]
        (if-let [current (get instruction-lines current-ins)]
          (let [updated-pointer (seti regs [current-ins 0 ip-reg])
                _ (if (= current-ins 28) (println regs current-ins))
                ins (str/split current #" ")
                ins-name (first ins)
                insv (mapv read-string (rest ins))
                evaled-state (call ins-name updated-pointer insv)]
            (recur evaled-state (inc (get evaled-state ip-reg)) (inc i)))
          [i regs]))))

(defn repeated-ins-8-to-12 [r3 r4]
  (bit-and (* (bit-and (+ r3 (bit-and r4 255)) 16777215) 65899) 16777215))

(defn generate-c-at-28 [input]
  (let [r4 (bit-or input 65536)
        r3 (repeated-ins-8-to-12 7041048 r4)
        r4-2 (int (Math/floor (/ r4 256)))
        r3-2 (repeated-ins-8-to-12 r3 r4-2)
        r4-3 (int (Math/floor (/ r4-2 256)))
        r3-3 (repeated-ins-8-to-12 r3-2 r4-3)]
    r3-3))

(defn last-c-value [start-value]
  (loop [v start-value all #{}]
    (let [new-value (generate-c-at-28 v)]
      (if (contains? all new-value) v
          (recur new-value (conj all new-value))))))

; Part 1
; Manual analysis of the puzzle input
; Hitting ins #28 will halt, if we have the right value at reg 0
; Hence we check what that value will be (= value of r3 when #28 is hit)
; Answer is 9107763 (after 1848 instructions)

; Part 2
; We create a generator function for r3 values at instruction 28
; Of these generated values, we find the last non-repeated value to answer part 2
#_(last-c-value 9107763)
