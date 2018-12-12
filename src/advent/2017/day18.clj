;; https://adventofcode.com/2017/day/18
(ns advent.2017.day18
  (:require [clojure.string :as str]))

(def puzzle-input
  (str/split-lines (slurp "inputs/2017/day18")))

(def test-input
  '("set a 1"
    "add a 2"
    "mul a a"
    "mod a 5"
    "snd a"
    "set a 0"
    "rcv a"
    "jgz a -1"
    "set a 1"
    "jgz a -2"))

(def test-input-2
  '("snd 1"
    "snd 2"
    "snd p"
    "rcv a"
    "rcv b"
    "rcv c"
    "rcv d"))

(defn parsed [input-lines]
  (zipmap (range 0 (count input-lines))
          (map #(str/split % #" ") input-lines)))

(def parsed-test (parsed test-input))
(def parsed-test-2 (parsed test-input-2))
(def parsed-puzzle (parsed puzzle-input))

(defn first-recov [parsed-input registers sound-stack current-ins-idx]
  (let [current (get parsed-input current-ins-idx)]
    (if (some? current)
      (let [current-ins (first current)
            operand-reg (second current)
            reg-value (get registers operand-reg 0)
            current-2 (get current 2)
            operand-val (if (some? current-2) (if (int? (read-string current-2)) (read-string current-2)
                                                  (get registers current-2 0)))]
        (condp = current-ins
          "snd" (first-recov parsed-input registers
                             (conj sound-stack reg-value)
                             (inc current-ins-idx))

          "rcv" (if (= 0 reg-value) (first-recov parsed-input registers
                                                 sound-stack (inc current-ins-idx))
                    (peek sound-stack))

          "set" (first-recov parsed-input (assoc registers operand-reg operand-val)
                             sound-stack (inc current-ins-idx))

          "add" (first-recov parsed-input (assoc registers operand-reg (+ reg-value operand-val))
                             sound-stack (inc current-ins-idx))

          "mul" (first-recov parsed-input (assoc registers operand-reg (* reg-value operand-val))
                             sound-stack (inc current-ins-idx))

          "mod" (first-recov parsed-input (assoc registers operand-reg (mod reg-value operand-val))
                             sound-stack (inc current-ins-idx))

          "jgz" (if (> reg-value 0) (first-recov parsed-input registers sound-stack (+ current-ins-idx operand-val))
                    (first-recov parsed-input registers sound-stack (inc current-ins-idx))))))))

(defn run-program [instructions registers current-ins-idx outgoing-queue incoming-queue]
  "returns [exit code, state (registers, current-ins-idx) outgoing queue, incoming queue].
   exit codes: 0 = instructions over, 1 = waiting on rcv."
  (let [current (get instructions current-ins-idx)]
    (if (some? current)
      (let [current-ins (first current)
            operand-reg (second current)
            reg-value (if (int? (read-string operand-reg)) (read-string operand-reg)
                          (get registers operand-reg 0))
            current-2 (get current 2)
            operand-val (if (some? current-2) (if (int? (read-string current-2)) (read-string current-2)
                                                  (get registers current-2 0)))]
        (condp = current-ins
          "snd" (run-program instructions registers (inc current-ins-idx)
                             (conj outgoing-queue reg-value) incoming-queue)

          "rcv" (if (empty? incoming-queue) [1 {:registers registers :current current-ins-idx}
                                             outgoing-queue incoming-queue]
                    (run-program instructions (assoc registers operand-reg (first incoming-queue))
                                 (inc current-ins-idx) outgoing-queue (vec (drop 1 incoming-queue))))

          "set" (run-program instructions (assoc registers operand-reg operand-val)
                             (inc current-ins-idx) outgoing-queue incoming-queue)

          "add" (run-program instructions (assoc registers operand-reg (+ reg-value operand-val))
                             (inc current-ins-idx) outgoing-queue incoming-queue)

          "mul" (run-program instructions (assoc registers operand-reg (* reg-value operand-val))
                             (inc current-ins-idx) outgoing-queue incoming-queue)

          "mod" (run-program instructions (assoc registers operand-reg (mod reg-value operand-val))
                             (inc current-ins-idx) outgoing-queue incoming-queue)

          "jgz" (if (> reg-value 0) (run-program instructions registers (+ current-ins-idx operand-val)
                                                 outgoing-queue incoming-queue)
                    (run-program instructions registers (inc current-ins-idx)
                                 outgoing-queue incoming-queue))))

      ; else => we have no instructions left
      [0 {:registers registers :current current-ins-idx} outgoing-queue incoming-queue])))

(defn run-both [instructions p0-state p1-state p0->p1 p1->p0 p1-outgoing]
  (let [[p0-code p0-new-state p0->p1-1 p1->p0-1] (run-program instructions (p0-state :registers)
                                                              (p0-state :current) p0->p1 p1->p0)
        [p1-code p1-new-state p1->p0-2 p0->p1-2] (run-program instructions (p1-state :registers)
                                                              (p1-state :current) p1->p0-1 p0->p1-1)
        p1-outgoing-new (+ p1-outgoing (- (count p1->p0-2) (count p1->p0-1)))
        are-queues-empty (and (empty? p1->p0-2) (empty? p0->p1-2))]
    (cond
      ; both have finished all instructions
      (= 0 p0-code p1-code) p1-outgoing-new
      ; deadlock
      (and (= 1 p0-code p1-code) are-queues-empty) p1-outgoing-new
      ; otherwise
      :else (run-both instructions p0-new-state p1-new-state p0->p1-2 p1->p0-2 p1-outgoing-new))))

; Part 1
(first-recov parsed-puzzle {} '() 0)

; Part 2
(run-both parsed-puzzle
          {:registers {"p" 0} :current 0}
          {:registers {"p" 1} :current 0} [] []
          0)
