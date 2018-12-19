(ns advent.2018.day19
  (:use [clojure.repl])
  (:require [clojure.string :as str]))

(def test-input
  ["#ip 0"
   "seti 5 0 1"
   "seti 6 0 2"
   "addi 0 1 0"
   "addr 1 2 3"
   "setr 1 0 0"
   "seti 8 0 4"
   "seti 9 0 5"])

(def puzzle-input (str/split-lines (slurp "inputs/2018/day19")))

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
  (when-let [fun (ns-resolve 'advent.2018.day19 (symbol name))]
    (apply fun args)))

(defmacro result [ins-name regs ins-vector]
  `((ns-resolve *ns* (symbol ~ins-name))
    ~regs
    ~ins-vector))

(defn regs-state [instruction-lines start-regs ip-reg limit]
  (loop [regs start-regs
         current-ins 0
         i 0]
    (if (> i limit) regs
        (if-let [current (get instruction-lines current-ins)]
          (let [updated-pointer (seti regs [current-ins 0 ip-reg])
                ins (str/split current #" ")
                _ (if (= current-ins 7) (println i updated-pointer))
                ins-name (first ins)
                insv (mapv read-string (rest ins))
                evaled-state (call ins-name updated-pointer insv)]
            ; (println updated-pointer current evaled-state)
            (recur evaled-state (inc (get evaled-state ip-reg)) (inc i)))
          regs))))

(defn divisors [n]
  (loop [i 1 result #{}]
    (if (> i (/ n 2)) result
        (recur (inc i)
               (if (= 0 (mod n i))
                 (conj (conj result (/ n i)) i)
                 result)))))

; Part 1
#_(regs-state (vec (rest puzzle-input))
              [0 0 0 0 0 0]
              5 10000000)

; Part 2
; The first part returns sum of divisors of the value in the
; register 1 (which is 867).
; For part 2 this value is 10551267
#_(reduce + (divisors 10551267))