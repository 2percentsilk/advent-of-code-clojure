(ns advent.2019.day3
  (:require [clojure.string :as str]))

(def puzzle-input 
  (map #(str/split % #",") (str/split-lines (slurp "inputs/2019/day3"))))

(def puzzle-line-1 (first puzzle-input))
(def puzzle-line-2 (second puzzle-input))

(defn new-start [direction length sx sy]
  (condp = direction
    "R" {:x (+ length sx) :y sy}
    "U" {:x sx :y (+ length sy)}
    "L" {:x (- sx length) :y sy}
    "D" {:x sx :y (- sy length)}))

(defn new-segments [segments direction length sx sy]
  (conj segments
        (condp = direction
          "R" {:dir "x" :y sy :x0 sx :x1 (+ sx length) :walk-dir direction}
          "U" {:dir "y" :x sx :y0 sy :y1 (+ sy length) :walk-dir direction}
          "L" {:dir "x" :y sy :x0 (- sx length) :x1 sx :walk-dir direction}
          "D" {:dir "y" :x sx :y0 (- sy length) :y1 sy :walk-dir direction})))

(defn parse-line-rec [raw {start-x :x start-y :y} segments]
  (if (empty? raw) segments
      (let [direction (subs (first raw) 0 1)
            length (read-string (subs (first raw) 1))
            remaining (rest raw)]
        (parse-line-rec remaining
                        (new-start direction length start-x start-y)
                        (new-segments segments direction length start-x start-y)))))

(defn parse-line [raw]
  (-> raw
      (parse-line-rec {:x 0 :y 0} '())
      reverse))

; (parse-line (str/split "R8,U5,L5,D3" #","))
; (parse-line (str/split "U7,R6,D4,L4" #","))

(defn x-segs [segments] (filter #(= "x" (:dir %)) segments))

(defn y-segs [segments] (filter #(= "y" (:dir %)) segments))

(defn cross-intersections-int [{:keys [y x0 x1]} ysegs]
  (->> ysegs
       (filter #(and (<= (:x %) x1) (>= (:x %) x0)))
       (filter #(and (<= y (:y1 %)) (>= y (:y0 %))))
       (map #(hash-map :x (:x %), :y y))))

; (cross-intersections-int {:dir "x", :y 0, :x0 0, :x1 8} '({:dir "y", :x 8, :y0 0, :y1 5}))

(defn cross-intersections [xsegs ysegs]
  (->> xsegs
       (map #(cross-intersections-int % ysegs))
       (apply concat)))

(defn intersection-pts [line-1 line-2]
  (concat (cross-intersections (x-segs line-1) (y-segs line-2))
          (cross-intersections (x-segs line-2) (y-segs line-1))))

; (intersection-pts (parse-line (str/split "R8,U5,L5,D3" #","))
;                   (parse-line (str/split "U7,R6,D4,L4" #",")))

(defn abs [n] (max n (- n)))

(defn manhattan [{:keys [x y]}] (+ (abs x) (abs y)))

; Part 1: 3247
(def part-1
  (->> (intersection-pts (parse-line puzzle-line-1)
                         (parse-line puzzle-line-2))
       (map manhattan)
       (filter #(not (= 0 %)))
       (apply min)))

(defn point-on-x-segment? [px py {:keys [y x0 x1]}]
  (and (= y py) (<= px x1) (>= px x0)))

(defn point-on-y-segment? [px py {:keys [x y0 y1]}]
  (and (= x px) (<= py y1) (>= py y0)))

(defn point-on-segment? [px py {dir :dir :as segment}]
  (condp = dir
    "x" (point-on-x-segment? px py segment)
    "y" (point-on-y-segment? px py segment)))

(defn length-xseg [{:keys [x0 x1]}] (abs (- x1 x0)))
(defn length-yseg [{:keys [y0 y1]}] (abs (- y1 y0)))

(defn length [{dir :dir :as segment}]
  (condp = dir 
    "x" (length-xseg segment)
    "y" (length-yseg segment)))

(defn dist-on-xseg [{:keys [walk-dir x0 x1]} px py]
  (condp = walk-dir
    "R" (abs (- px x0))
    "L" (abs (- x1 px))))

(defn dist-on-yseg [{:keys [walk-dir y0 y1]} px py]
  (condp = walk-dir
    "U" (abs (- py y0))
    "D" (abs (- y1 py))))

(defn distance-on-segment [{dir :dir :as segment} px py]
  (condp = dir
    "x" (dist-on-xseg segment px py)
    "y" (dist-on-yseg segment px py)))

(defn steps-on-line [{:keys [x y]} segments]
  (loop [remaining segments steps 0]
    (if (not (point-on-segment? x y (peek remaining))) 
      (recur (pop remaining) (+ steps (length (peek remaining))))
      (+ steps (distance-on-segment (peek remaining) x y)))))

; (steps-on-line {:x 3 :y 3}
;                (parse-line (str/split "R8,U5,L5,D3" #",")))

(defn steps [pt line-1 line-2]
  (+ (steps-on-line pt line-1) (steps-on-line pt line-2)))

; (steps {:x 3 :y 3}
;        (parse-line (str/split "R8,U5,L5,D3" #","))
;        (parse-line (str/split "U7,R6,D4,L4" #",")))

; Part 2: 48054
; "Elapsed time: 52.47062 msecs"
(time
 (def part-2
   (->> (intersection-pts (parse-line puzzle-line-1) (parse-line puzzle-line-2))
        (map #(steps % (parse-line puzzle-line-1) (parse-line puzzle-line-2)))
        (filter #(not (= 0 %)))
        (apply min)))
 )
