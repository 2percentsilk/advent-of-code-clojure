(ns advent.2019.day10
  (:require [clojure.string :as str])
  (:require [clojure.math.numeric-tower :as math]))

(def test-input
  '(".#..#"
    "....."
    "#####"
    "....#"
    "...##"))

(def test-2
  '(".#..##.###...#######"
    "##.############..##."
    ".#.######.########.#"
    ".###.#######.####.#."
    "#####.##.#.##.###.##"
    "..#####..#.#########"
    "####################"
    "#.####....###.#.#.##"
    "##.#################"
    "#####.##.###..####.."
    "..######..##.#######"
    "####.##.####...##..#"
    ".#####..#.######.###"
    "##...#.##########..."
    "#.##########.#######"
    ".####.#.###.###.#.##"
    "....##.##.###..#####"
    ".#.#.###########.###"
    "#.#.#.#####.####.###"
    "###.##.####.##.#..##"))

(def test-3 
  '(".#....#####...#.."
    "##...##.#####..##"
    "##...#...#.#####."
    "..#.....#...###.."
    "..#.#.....#....##"))

(def puzzle-input (str/split-lines (slurp "inputs/2019/day10")))

(defn parse-line [line line-idx]
  (->> (str/split line #"")
       (map-indexed (fn [idx itm] (if (= itm "#") (vector idx line-idx) nil)))
       (filter #(not= nil %))))

(defn parse [input]
  (->> input
       (map-indexed (fn [idx itm] (parse-line itm idx)))
       (apply concat)))

(defn round [x] (/ (math/round (* 10000000 x)) 10000000))
(defn subtract [[x0 y0] [x1 y1]] (vector (- x0 x1) (- y0 y1)))
(defn add [[x0 y0] [x1 y1]] (vector (+ x0 x1) (+ y0 y1)))

(defn length [x y] (math/sqrt (+ (* x x) (* y y))))

(defn projection [[x y]] [(round (/ x (length x y)))
                          (round (/ y (length x y)))])

(defn visible-asteroids [layout pt]
  (->> layout
       (map #(subtract % pt))
       (filter #(not= % [0 0]))
       (map #(projection %))
       distinct count))

(defn max-visible-asteroids-pt [layout]
  (apply max-key #(visible-asteroids layout %) layout))

(defn max-visible-asteroids [layout]
  (visible-asteroids layout (max-visible-asteroids-pt layout)))

; Part 1: 280
#_(def part-1 (max-visible-asteroids (parse puzzle-input)))

(defn cosine [[x0 y0] [x1 y1]]
  (/ (+ (* x0 x1) (* y0 y1))
     (* (length x0 y0) (length x1 y1))))

(defn cross [[x0 y0] [x1 y1]]
  "returns 1 when clockwise, else -1"
  (if (>= (* x0 y1) (* x1 y0)) 1 -1))

(defn cosine-with-proj [p0 p1] (cosine (projection p0) (projection p1)))
(defn cross-with-proj [p0 p1] (cross (projection p0) (projection p1)))

(defn clockwise [p0 p1] (* (cross-with-proj p0 p1) (round (cosine-with-proj p0 p1))))

(clockwise [1 1] [1 1])
(clockwise [1 1] [1 0])
(clockwise [1 1] [-1 1/2])

(defn prepare [center layout]
  (->> layout
       (map #(subtract % center))
       (filter #(not= % [0 0]))))
  
(defn kill-once [gun-xy center layout line-blocked]
  (->> layout
       (prepare center)
       (filter #(if line-blocked (not= 1 (clockwise (subtract gun-xy center) %)) true))
       (apply max-key #(clockwise (subtract gun-xy center) %))
       (add center)
       ))

(defn kill-n [layout n]
  (let [center (max-visible-asteroids-pt layout)]
    (loop [i 0
           gun-xy (add center [0 -1])
           line-blocked false
           rem layout]
      (println "----" i gun-xy)
      (if (>= i n) gun-xy
          (let [res (kill-once gun-xy center rem line-blocked)]
            (recur (inc i) res true (filter #(not= % res) rem)))))))

(assert (= [8 1] (kill-n (parse test-3) 1)))
(assert (= [9 0] (kill-n (parse test-3) 2)))
(assert (= [11 2] (kill-n (parse test-3) 8)))

(kill-n (parse test-2) 10)

(clockwise [1 -7] [1 -6])
(clockwise [1 -7] [1 -5])
(clockwise [1 -7] [2 -13])

(clockwise [2 -13] [-1 6])
(clockwise [2 -13] [1 -6])

; (clockwise [1 -12] [1 -11])

(defn safe-div [y x]
  (cond 
    (not= 0 x) (/ y x)
    (> y 0) ##Inf
    (< y 0) ##-Inf))

(defn deg [rad] (/ (* 180 rad) Math/PI))

(defn tan-i [[x y]]
  (let [atan (deg (Math/atan (safe-div y x)))]
    (if (>= x 0) atan 
        (+ 180 atan))))
