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
          "R" {:dir "x" :y sy :x0 sx :x1 (+ sx length)}
          "U" {:dir "y" :x sx :y0 sy :y1 (+ sy length)}
          "L" {:dir "x" :y sy :x0 (- sx length) :x1 sx}
          "D" {:dir "y" :x sx :y0 (- sy length) :y1 sy})))

(defn parse-line-rec [raw {start-x :x start-y :y} segments]
  (if (empty? raw) segments
      (let [direction (subs (first raw) 0 1)
            length (read-string (subs (first raw) 1))
            remaining (rest raw)]
        (parse-line-rec remaining
                        (new-start direction length start-x start-y)
                        (new-segments segments direction length start-x start-y)))))

(defn parse-line [raw] (parse-line-rec raw {:x 0 :y 0} '()))

(parse-line (str/split "R75,D30,R83,U83,L12,D49,R71,U7,L72" #","))

(defn x-segs [segments] (filter #(= "x" (:dir %)) segments))

(defn y-segs [segments] (filter #(= "y" (:dir %)) segments))

(defn point-on-x-segments? [segments px py]
  (->> segments
       x-segs
       (filter #(and (= py (:y %)) (>= px (:x0 %)) (<= px (:x1 %))))
       empty?
       not))

(defn point-on-y-segments? [segments px py]
  (->> segments
       y-segs
       (filter #(and (= px (:x %)) (>= py (:y0 %)) (<= py (:y1 %))))
       empty?
       not))

(defn point-on-line? [line point-x point-y]
  (or (point-on-x-segments? line point-x point-y)
      (point-on-y-segments? line point-x point-y)))

(defn segment-x-pts [{:keys [y x0 x1] :as segment}]
  (if (> x0 x1) '() 
      (conj (segment-x-pts (assoc segment :x0 (inc x0))) {:x x0 :y y})))

(defn segment-y-pts [{:keys [x y0 y1] :as segment}]
  (if (> y0 y1) '()
      (conj (segment-y-pts (assoc segment :y0 (inc y0))) {:x x :y y0})))

(defn segment-pts [{dir :dir :as segment}]
  (condp = dir
    "x" (segment-x-pts segment)
    "y" (segment-y-pts segment)))

(defn abs [n] (max n (- n)))

(defn new-commons [commons line px py]
  (if (point-on-line? line px py) 
    (conj commons (+ (abs px) (abs py))) 
    commons))

(defn find-commons [line-1 line-2]
  (loop [pts (segment-pts (peek line-1))
         remaining (pop line-1)
         commons '()]
    (cond 
      (and (empty? pts) (empty? remaining)) commons
      (empty? pts) (recur (segment-pts (peek remaining))
                          (pop remaining) 
                          commons)
      :else (recur (pop pts) 
                   remaining 
                   (new-commons commons line-2 (:x (peek pts)) (:y (peek pts)))))))

; (find-commons (parse-line '("R8" "U5" "L5" "D3"))
;               (parse-line '("U7" "R6" "D4" "L4")))

; (find-commons (parse-line (str/split "U62,R66,U55,R34,D71,R55,D58,R83" #","))
;               (parse-line (str/split "R75,D30,R83,U83,L12,D49,R71,U7,L72" #",")))

; (find-commons (parse-line (str/split "R75,D30,R83,U83,L12,D49,R71,U7,L72" #","))
;               (parse-line (str/split "U62,R66,U55,R34,D71,R55,D58,R83" #",")))


; (find-commons (parse-line (str/split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" #","))
;               (parse-line (str/split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" #",")))

(def part-1
  (->> (find-commons (parse-line puzzle-line-1)
                     (parse-line puzzle-line-2))
       (filter #(not (= 0 %)))
       (apply min)))

(defn -main [] (println part-1))
