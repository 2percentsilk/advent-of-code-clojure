;; https://adventofcode.com/2018/day/7
(ns advent.2018.day7.part1
  (:require [clojure.string :as str]))

(defn start [input-string]
  (subs input-string 5 6))

(defn end [input-string]
  (subs input-string 36 37))

(def test-edges-input
  '("Step C must be finished before step F can begin."
    "Step C must be finished before step A can begin."
    "Step A must be finished before step B can begin."))

(defn filter-input [input-strings start-step]
  (filter #(= (start %) start-step) input-strings))

(defn sub-graph [input-strings start-step]
  (let [filtered-input (filter-input input-strings start-step)
        ends (map end filtered-input)]
    (hash-map start-step (vec (sort ends)))))

(assert (= {"C" ["A" "F"]} (sub-graph test-edges-input "C")))

(defn graph [input-strings]
  (let [starts (set (map start input-strings))
        sub-graphs (map #(sub-graph input-strings %) starts)]
    (apply merge sub-graphs)))

(assert (= {"C" ["A" "F"] "A" ["B"]} (graph test-edges-input)))

(defn is-root [input-graph start-step]
  (not (some #(.contains % start-step) (vals input-graph))))

(defn find-roots [input-graph]
  (vec (sort (filter #(is-root input-graph %) (keys input-graph)))))

(assert (= ["C"] (find-roots (graph test-edges-input))))

(defn new-roots [existing-roots input-graph]
  (let [root-key (get existing-roots 0)
        root-val (get input-graph root-key)
        root-check-graph (dissoc input-graph root-key)
        valid-root-val (filter #(is-root root-check-graph %) root-val)]
    (vec (sort (concat (subvec existing-roots 1) valid-root-val)))))

(assert (= ["B" "F"] (new-roots ["A" "F"] (graph test-edges-input))))

(defn steps-in-order [input-graph]
  (loop [new-graph input-graph
         roots (find-roots new-graph)
         steps ""]
    (cond
      (empty? roots) steps
      :else (recur (dissoc new-graph (get roots 0))
                   (new-roots roots new-graph)
                   (str steps (get roots 0))))))

(assert (= "CABF" (steps-in-order (graph test-edges-input))))

(def test-input-2
  ["Step C must be finished before step A can begin."
   "Step C must be finished before step F can begin."
   "Step A must be finished before step B can begin."
   "Step A must be finished before step D can begin."
   "Step B must be finished before step E can begin."
   "Step D must be finished before step E can begin."
   "Step F must be finished before step E can begin."])

(assert (= "CABDFE" (steps-in-order (graph test-input-2))))

(def puzzle-input
  (str/split-lines (slurp "inputs/2018/day7")))

(defn -main []
  (println (steps-in-order (graph puzzle-input))))
