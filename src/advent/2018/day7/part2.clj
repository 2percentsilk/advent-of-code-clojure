;; https://adventofcode.com/2018/day/7#part2
(ns advent.2018.day7.part2
  (:use [advent2018.puzzle7.part1 :exclude (-main)]))

(defn job-duration [character duration-offset]
  (+ (- (int (first character)) 64) duration-offset))

(defn initial-worker-state [num-workers]
  (zipmap (range num-workers) (repeat num-workers nil)))

(assert (= {0 nil 1 nil} (initial-worker-state 2)))

; worker state looks like worker-id and assigned-job --> {0 ["C" 3]}
; assigned-job looks like job-id and time-left
; at every iteration, we decrement the time left values
; if a time left is 0, then the job is done
; next job to do = longest job among available roots (last of roots vector)

(defn is-worker-available [worker-id worker-state]
  (let [worker-val (worker-state worker-id)]
  (or (nil? worker-val) (= 0 (last worker-val)))))

(defn dec-job-time-left [worker-val]
  (if (nil? worker-val) nil (update worker-val 1 dec)))

(defn dec-time-left [worker-state]
  (zipmap (keys worker-state) (map dec-job-time-left (vals worker-state))))

(defn assign-job [worker-state worker-id job-id job-cost]
  (assoc worker-state worker-id [job-id job-cost]))

(defn remove-done-jobs [worker-state input-graph]
  (let [done-job-ids (map first (filter #(= 0 (last %)) (vals worker-state)))
        incomplete-job-ids (filter #(not (.contains done-job-ids %)) (keys input-graph))]
    (select-keys input-graph incomplete-job-ids)))

(assert (= {"F" ["G" "H"]} (remove-done-jobs {0 ["C" 0] 1 ["A" 1]} {"C" ["F"] "F" ["G" "H"]})))

(defn jobs-to-assign [input-graph worker-state]
  "return available roots from input graph which are not ongoing"
  (let [ongoing-jobs (map first (vals worker-state))]
    (filter #(not (.contains ongoing-jobs %)) (find-roots input-graph))))

(defn remove-zero-time-left [worker-state]
  (zipmap (keys worker-state) (map #(if (= 0 (last %)) nil %) (vals worker-state))))

(defn update-state [input-worker-state input-graph duration-offset count-value]
  (let [dec-state (dec-time-left input-worker-state)
        remaining-graph (remove-done-jobs dec-state input-graph)
        new-state (remove-zero-time-left dec-state)
        available-workers (filter #(is-worker-available % new-state) (keys new-state))]
    (loop [workers available-workers
           worker-state new-state
           current-graph remaining-graph]
      (let [jobs (jobs-to-assign current-graph worker-state)]
        (cond
          (or (empty? workers) (empty? jobs)) (vector worker-state
                                                      current-graph
                                                      (inc count-value))
          :else (recur (drop 1 workers)
                       (assign-job worker-state (first workers) (last jobs) (job-duration (last jobs) duration-offset))
                       current-graph))
        ))))

(defn do-work [input-graph num-workers duration-offset]
  (loop [worker-state (initial-worker-state num-workers)
         current-graph input-graph
         count -1]
    (let [[new-worker-state new-graph new-count] (update-state worker-state current-graph duration-offset count)]
      (cond
        (empty? current-graph) count
        :else (recur new-worker-state new-graph new-count)))))

(defn add-leaves [input-graph]
  (let [all-job-ids (into #{} (flatten (vals input-graph)))]
    (reduce #(assoc %1 %2 (get %1 %2 [])) input-graph all-job-ids)))

(assert (= 15 (do-work (add-leaves (graph test-input-2)) 2 0)))

(defn -main []
  (println (do-work (add-leaves (graph puzzle-input)) 5 60)))
