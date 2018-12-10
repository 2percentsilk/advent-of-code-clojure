;; https://adventofcode.com/2017/day/7
(ns advent.2017.day7
  (:require [clojure.string :as str]))

(def puzzle-input
  (->> (slurp "inputs/2017/day7")
       (str/split-lines)
       (map #(str/split % #" "))))

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

(defn find-root [input-tree]
  (let [all-children (set (reduce concat (map #(% :children) (vals input-tree))))
        all-keys (keys input-tree)
        root (filter #(not (contains? all-children %)) all-keys)]
    root))

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

(defn inequality-tree [tree-weights]
  "at every level, check if children have same sum. if not,
  check deeper, and return the deepest such inequality."
  (let [children (tree-weights :c)
        children-sum (map #(% :sum) children)
        have-same (apply = children-sum)]
    (if have-same nil

                  ; find the different child, go deeper
                  (let [same-value (first (apply max-key val (frequencies children-sum)))
                        child (first (filter #(not (= same-value (% :sum))) children))
                        child-inequality (inequality-tree child)]

                    {:label (child :label)
                     :sum (child :sum)
                     :w (child :w)
                     :expected-sum same-value
                     :c child-inequality}
                  ))))

(defn deepest [inequality]
  (if (empty? (inequality :c)) inequality
                               (deepest (inequality :c))))

(defn true-weight [dpst]
  (- (dpst :w) (- (dpst :sum) (dpst :expected-sum))))

; Part 1
(find-root (tree puzzle-input))

; Part 2
(def t (tree puzzle-input))
(def r (first (find-root t)))

(true-weight
  (deepest
    (inequality-tree
      (weights t r))))
