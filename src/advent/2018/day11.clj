;; https://adventofcode.com/2018/day/11
;; Puzzle input: 7400
(ns advent.2018.day11)

(defn hundredth [num]
  (int (mod (Math/floor (/ num 100)) 10)))

(defn cell-value [x y serial-num]
  (- (hundredth (* (+ (* (+ x 10) y) serial-num) (+ x 10))) 5))

(def m-cell-value (memoize cell-value))

(defn sum-grid-at [x y serial-num size]
  (let [xs (map + (range 0 size) (repeat size x))
        ys (map + (range 0 size) (repeat size y))
        all (for [x1 xs y1 ys] (vector x1 y1))]
    (reduce + (map (fn [[x y]] (m-cell-value x y serial-num)) all))))

(defn max-grid [grids]
  "grid is a hash-map with one key: {[x y] sum}"
  (let [non-nil (filter #(not (nil? %)) grids)]
    (if (empty? non-nil) nil
                         (into {} [(apply max-key val (map first non-nil))]))))

(defn largest-sub-grid [x y limit serial-num]
  "returns {[x y] sum} of largest known grid inside"
  (cond
    ; corners
    (< (- limit x) 2) nil
    (< (- limit y) 2) nil

    :else (let [here-sum (sum-grid-at x y serial-num 3)
                g1 (largest-sub-grid (inc x) y limit serial-num)
                g2 (largest-sub-grid x (inc y) limit serial-num)
                g3 (largest-sub-grid (inc x) (inc y) limit serial-num)

                m (max-grid [g1 g2 g3])
                m-sum (if (nil? m) nil (first (vals m)))]

            (if (nil? m) {[x y] here-sum}
                         (if (< m-sum here-sum) {[x y] here-sum}
                                                m)))
    ))

(defn largest-sub-grid-at [[x y] limit serial-num]
  "returns [size sum] of largest grid starting at x, y"
  (cond
    (= x limit) [1 (m-cell-value x y serial-num)]
    (= y limit) [1 (m-cell-value x y serial-num)]

    :else (let [[inner-size inner-sum]
                (largest-sub-grid-at [(inc x) (inc y)] limit serial-num)

                cell-value (m-cell-value x y serial-num)

                row-sum (reduce + (map #(m-cell-value % y serial-num)
                                       (range (inc x) (+ inner-size (inc x)))))
                col-sum (reduce + (map #(m-cell-value x % serial-num)
                                       (range (inc y) (+ inner-size (inc y)))))

                total-sum (+ inner-sum cell-value row-sum col-sum)]
            (if (> cell-value total-sum) [1 (m-cell-value x y serial-num)]
                                         [(inc inner-size) total-sum]))))

(defn find-best-xy [limit serial-num]
  "returns [[x y] [size sum]]"
  (let [xs (range 1 (inc limit))
        ys (range 1 (inc limit))
        xy (for [x xs y ys] (vector x y))]
    (apply max-key #(second (val %))
           (zipmap xy (map #(largest-sub-grid-at % limit serial-num)
                           xy)))))

; Part 1
; "Elapsed time: 1140.389827 msecs"
; Answer: {[34 72] 29}
#_(with-redefs [largest-sub-grid (memoize largest-sub-grid)]
    (largest-sub-grid 1 1 300 7400))

; Part 2
; Answer: [[233 187] [13 91]]
#_(with-redefs [largest-sub-grid-at (memoize largest-sub-grid-at)]
    (find-best-xy 300 7400))
