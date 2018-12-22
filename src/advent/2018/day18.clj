(ns advent.2018.day18
  (:require [clojure.string :as str]))

(def test-input
  '(".#.#...|#."
    ".....#|##|"
    ".|..|...#."
    "..|#.....#"
    "#.#|||#|#|"
    "...#.||..."
    ".|....|..."
    "||...#|.#|"
    "|.||||..|."
    "...#.|..|."))

(def puzzle-input (str/split-lines (slurp "inputs/2018/day18")))

(defn parse-line [input y]
  (into {} (map-indexed #(assoc {} [%1 y] %2) input)))

(defn parse-board [input]
  (into {} (map-indexed #(parse-line %2 %1) input)))

(defn adjacents [[x y] board]
  (let [all (for [x1 (range (dec x) (+ x 2))
                  y1 (range (dec y) (+ y 2))] (vector x1 y1))
        wo-self (filter #(not (= [x y] %)) all)
        values (->> wo-self
                    (filter #(some? (get board %)))
                    (map #(board %)))]
    (frequencies values)))

(defn next-state [board [x y]]
  (let [v (get board [x y])
        fs (adjacents [x y] board)]
    (condp = v
      \. (if (>= (get fs \| 0) 3) \| \.)
      \| (if (>= (get fs \# 0) 3) \# \|)
      \# (if (and (>= (get fs \# 0) 1)
                  (>= (get fs \| 0) 1)) \# \.))))

(defn next-board-state [board xm ym]
  (into {} (for [x0 (range 0 xm) y0 (range 0 ym)]
             (assoc {} [x0 y0] (next-state board [x0 y0])))))

(defn print-board-state [board xm ym]
  (loop [x 0 y 0]
    (cond
      (>= y ym) (do
                  (println))
      (>= x xm) (do
                  (println)
                  (recur 0 (inc y)))
      :else (do
              (print (get board [x y]))
              (recur (inc x) y)))))

(defn res-value [board]
  (let [fv (frequencies (vals board))]
    (* (get fv \| 0) (get fv \# 0))))

(defn board-state-after [board [boardx boardy] minutes]
  (loop [state board i 0]
    (print-board-state state boardx boardy)
    (if (>= i minutes) state
        (recur (next-board-state state boardx boardy) (inc i)))))

; Part 1
; Run for 10 minutes
#_(res-value (board-state-after (parse-board puzzle-input)
                                [50 50] 10))

; Part 2
; Run for 1,000,000,000 minutes
; Solution: Pattern that runs every 28 minutes
; minutes res-value
; 472 195460
; 473 191285
; 474 185004
; 475 181192
; 476 176484
; 477 178776
; 478 177232
; 479 179800
; 480 181853
; 481 185859
; 482 187850
; 483 190046
; 484 190740
; 485 186595
; 486 182560
; 487 184008
; 488 184032
; 489 184254
; 490 186880
; 491 191160
; 492 195000
; 493 198387
; 494 201798
; 495 201798
; 496 201465
; 497 199995
; 498 200178
; 499 197232

; Find corresponding minute for 1000000000
#_(+ (mod 1000000000 28) 476)
