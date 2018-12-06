;; https://adventofcode.com/2018/day/5
(ns puzzle5.part1)

(defn is-reaction [f s]
  (cond
    (or (nil? f) (nil? s)) false
    :else (= 32 (Math/abs (- (int f) (int s))))))

(assert (= true (is-reaction \a \A)))
(assert (= false (is-reaction \a \a)))
(assert (= false (is-reaction \a nil)))

(defn stack-reducer [stack current-char]
  (let [prev-char (peek stack)
        reacted (is-reaction prev-char current-char)]
    (if reacted (pop stack) (conj stack current-char))))

(assert (= '(\a) (stack-reducer '() \a)))
(assert (= '() (stack-reducer '(\a) \A)))

(defn result [input]
  (reduce stack-reducer '() input))

(assert (= () (result "aAbB")))

(def loaded-string (slurp "inputs/puzzle5"))

(defn -main []
  (println (count (result loaded-string))))
