;; https://adventofcode.com/2017/day/9
(ns advent2018.2017.day9
  (:require [clojure.string :as str]))

(defn sum-groups [group-result]
  (reduce + (get group-result 0)))

(defn groups [input]
  (loop [remaining input
         result '()
         nesting-level 1
         garbage-chars 0
         group-stack '()
         is-in-garbage false
         is-after-! false]
    (cond
      (empty? remaining) [result garbage-chars]

      is-after-! (recur (rest remaining)
                        result
                        nesting-level
                        garbage-chars
                        group-stack
                        is-in-garbage
                        false)

      ; guaranteed to be not after ! -->
      is-in-garbage (let [current (first remaining)]
                      (cond
                        (= current "!") (recur (rest remaining)
                                               result
                                               nesting-level
                                               garbage-chars
                                               group-stack
                                               is-in-garbage
                                               true)

                        (= current ">") (recur (rest remaining)
                                               result
                                               nesting-level
                                               garbage-chars
                                               group-stack
                                               false
                                               is-after-!)
                        :else (recur (rest remaining)
                                     result
                                     nesting-level
                                     (inc garbage-chars)
                                     group-stack
                                     is-in-garbage
                                     is-after-!)))

      ; guaranteed to be not in garbage -->
      :else (let [current (first remaining)]
              (cond
                (= current "{") (recur (rest remaining)
                                       result
                                       (inc nesting-level)
                                       garbage-chars
                                       (conj group-stack nesting-level)
                                       is-in-garbage
                                       is-after-!)

                (= current "}") (recur (rest remaining)
                                       (conj result (peek group-stack))
                                       (dec nesting-level)
                                       garbage-chars
                                       (pop group-stack)
                                       is-in-garbage
                                       is-after-!)

                (= current "<") (recur (rest remaining)
                                       result
                                       nesting-level
                                       garbage-chars
                                       group-stack
                                       true
                                       is-after-!)

                ; ignore other characters
                :else (recur (rest remaining)
                             result
                             nesting-level
                             garbage-chars
                             group-stack
                             is-in-garbage
                             is-after-!)
                )))))

(def puzzle-input (slurp "inputs/2017/puzzle9"))

; Part 1
(sum-groups (groups (str/split puzzle-input #"")))

; Part 2
(get (groups (str/split puzzle-input #"")) 1)
