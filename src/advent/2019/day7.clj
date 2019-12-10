(ns advent.2019.day7
  (:require [advent.2019.day5 :as base])
  (:require [clojure.math.combinatorics :as combo]))

(def puzzle-input (slurp "inputs/2019/day7"))

(def test-program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
(def test-phase '(4 3 2 1 0))

(defn run-amplifier [program phase input]
  "runs one number, the output of the executions on an amplifier"
  (-> program
      base/parse-input
      (base/build-state (conj '() input phase))
      base/run-all
      :outputs
      first))

(defn run-all-amplifiers [program phases current-input]
  (if (empty? phases)
    current-input
    (run-all-amplifiers program (rest phases)
                        (run-amplifier program (first phases) current-input))))

(defn all-phases [phase-start n-amps]
  (map seq (combo/permutations (range phase-start (+ phase-start n-amps)))))

(defn highest-output [program start-input n-amps phase-start]
  (loop [phases (all-phases phase-start n-amps)
         best-so-far 0]
    (if (empty? phases) best-so-far
        (recur (rest phases)
               (let [res (run-all-amplifiers program 
                                             (first phases) 
                                             start-input)]
                 (if (> res best-so-far) res best-so-far))))))

; Part 1: 844468
#_(def part-1
    (highest-output puzzle-input 0 5 0))

; (highest-output
;  "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
;  0 5 5)

; Feedback loop mode
; (defn run-one-step [program-state])

; (defn run-all-feedback [program phases current-input]
;   )