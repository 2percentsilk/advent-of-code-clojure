(ns advent.2018.day20
  (:require [clojure.string :as str]))

(def test-input "^ENWWW(NEEE|SSE(EE|N))$")
(def test-input-2 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")

(defn letter? [c]
  (or (= c \W)
      (= c \E)
      (= c \N)
      (= c \S)))

(defn get-options [pattern]
  (loop [remaining pattern
         options []
         current-opt []
         brack-stack '()]
    (if-let [c (first remaining)]
      (cond
        (= c \() (recur (rest remaining)
                        options
                        (if (not (empty? brack-stack)) (conj current-opt c) current-opt)
                        (conj brack-stack c))

        (= c \)) (if (= brack-stack '(\())
                   ; only one open bracket in the stack -> we are done with all options
                   (->> current-opt
                        (conj options)
                        (mapv #(into % (rest remaining))))
                   ; else
                   (recur (rest remaining)
                          options
                          (conj current-opt c)
                          (rest brack-stack)))

        (= c \|) (if (= brack-stack '(\())
                   (recur (rest remaining) (conj options current-opt) [] brack-stack)
                   (recur (rest remaining) options (conj current-opt c) brack-stack))

        :else (recur (rest remaining) options (conj current-opt c) brack-stack))
      options)))

(defn opposites? [c1 c2]
  (let [s (set (vector c1 c2))]
    (or (= s #{\N \S})
        (= s #{\E \W}))))

(defn furthest-room [pattern moves-stack shortest-so-far]
  (if-let [c (first pattern)]
    (cond
      (= c \^) (furthest-room (rest pattern) moves-stack shortest-so-far)
      (= c \$) shortest-so-far
      (letter? c) (furthest-room (rest pattern)
                                 (if (opposites? (first moves-stack) c) (rest moves-stack)
                                     (conj moves-stack c))
                                 (if (opposites? (first moves-stack) c) (dec shortest-so-far)
                                     (inc shortest-so-far)))
      (= c \() (->> pattern
                    get-options
                    (mapv #(furthest-room % moves-stack shortest-so-far))
                    (apply max))
      :else shortest-so-far)))

(def puzzle-input (slurp "inputs/2018/day20"))

#_(furthest-room puzzle-input '() 0)