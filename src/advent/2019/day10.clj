(ns advent.2019.day10
  (:require [clojure.string :as str]))

(def test-input
  '(".#..#"
    "....."
    "#####"
    "....#"
    "...##"))

(def test-2
  '(".#..#..###"
    "####.###.#"
    "....###.#."
    "..###.##.#"
    "##.##.#.#."
    "....###..#"
    "..#.#..#.#"
    "#..#.#.###"
    ".##...##.#"
    ".....#.#.."))

(def puzzle-input (str/split-lines (slurp "inputs/2019/day10")))

(defn parse-line [line line-idx]
  (->> (str/split line #"")
       (map-indexed (fn [idx itm] (if (= itm "#") (vector idx line-idx) nil)))
       (filter #(not= nil %))))

(defn parse [input]
  (->> input
       (map-indexed (fn [idx itm] (parse-line itm idx)))
       (apply concat)))

(defn abs [x] (max x (- x)))
(defn subtract [[x0 y0] [x1 y1]] (vector (- x0 x1) (- y0 y1)))
(defn abs-slope [[x y]] (if (not= x 0) (abs (/ y x)) "inf"))

(defn dcount [pts]
  (->> pts (map #(abs-slope %)) distinct count))

(defn dcount-wrapper [pts]
  (+ (dcount (filter (fn [[x y]] (and (>= x 0) (>= y 0))) pts))
     (dcount (filter (fn [[x y]] (and (>= x 0) (< y 0))) pts))
     (dcount (filter (fn [[x y]] (and (< x 0) (>= y 0))) pts))
     (dcount (filter (fn [[x y]] (and (< x 0) (< y 0))) pts))))

(defn visible-asteroids [layout pt]
  (->> layout
       (map #(subtract % pt))
       (filter #(not= % [0 0]))
       dcount-wrapper))

(defn max-visible-asteroids-pt [layout]
  (apply max-key #(visible-asteroids layout %) layout))

(defn max-visible-asteroids [layout]
  (visible-asteroids layout (max-visible-asteroids-pt layout)))

; Part 1: 280
#_(def part-1 (max-visible-asteroids (parse puzzle-input)))

