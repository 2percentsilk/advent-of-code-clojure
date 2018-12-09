;; https://adventofcode.com/2017/day/7
(ns advent2018.2017.day7
  (:require [clojure.string :as str]))

(def puzzle-input
  (->> (slurp "inputs/2017/puzzle7")
       (str/split-lines)
       (map #(str/split % #" "))))

(println puzzle-input)

(defn trim-commas [input]
  (if (>= (.indexOf input ",") 0)
    (subs input 0 (dec (.length input)))
    input))

(defn get-children [input]
  (let [has-child (> (.length input) 2)]
    (if has-child (map trim-commas (subvec input 3)) [])))

(defn get-weight [input]
  (let [weight-str (second input)]
    (read-string (subs weight-str 1 (dec (.length weight-str))))))

(defn tree [input]
  (into {} (map #(assoc {} (first %)
                           {:weight (get-weight %) :children (get-children %)})
                input)))

(tree puzzle-input)

(defn find-root [input-tree]
  (let [all-children (set (reduce concat (map #(% :children) (vals input-tree))))
        all-keys (keys input-tree)
        root (filter #(not (contains? all-children %)) all-keys)]
    root))

(find-root (tree puzzle-input))

(defn weights [tree root]
  (let [value (get tree root)
        children (value :children)
        self-weight (value :weight)
        has-no-children (empty? children)]
    (if has-no-children

      {:sum self-weight :label root :w self-weight :c []}

      (let [children-result (map #(weights tree %) children)
            children-weights (map #(% :sum) children-result)]
        {:w     self-weight
         :label root
         :c     children-result
         :sum   (reduce + self-weight children-weights)}
        ))
    ))

(result (weights (tree puzzle-input) "eugwuhl"))

(defn result [weights]
  "verify that children have same sum"
  (let [children (weights :c)
        have-same-sum (apply = (map #(% :sum) children))]
    (if have-same-sum true
                      (println (map #(% :sum) children)))
    ))

(weights (tree puzzle-input) "drjmjug")

(def t (tree puzzle-input))

(map #(weights t %) ((get (tree puzzle-input) "drjmjug") :children))