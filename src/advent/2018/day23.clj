(ns advent.2018.day23
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.data.priority-map :as pm]))

(def puzzle-input
  (->> "inputs/2018/day23"
       slurp
       str/split-lines))

(def test-input
  '("pos=<0,0,0>, r=4"
    "pos=<1,0,0>, r=1"
    "pos=<4,0,0>, r=3"
    "pos=<0,2,0>, r=1"
    "pos=<0,5,0>, r=3"
    "pos=<0,0,3>, r=1"
    "pos=<1,1,1>, r=1"
    "pos=<1,1,2>, r=1"
    "pos=<1,3,1>, r=1"))

(def test-input-2
  '("pos=<10,12,12>, r=2"
    "pos=<12,14,12>, r=2"
    "pos=<16,12,12>, r=4"
    "pos=<14,14,14>, r=6"
    "pos=<50,50,50>, r=200"
    "pos=<10,10,10>, r=5"))

(def test-line
  "pos=<-14265865,33121875,56157982>, r=89946524")

(defn parse-pos [p]
  (let [len (count p)]
    (mapv read-string (str/split (subs p 1 (dec len)) #","))))

(defn parse-line [l]
  (let [[p r] (str/split l #", ")]
    {:p (parse-pos (subs p 4))
     :r (read-string (subs r 2))}))

(defn parsed [input]
  (->> input
       (map parse-line)))

(defn max-radius [input]
  (first (sort-by :r > input)))

(defn distance [[x0 y0 z0] [x1 y1 z1]]
  (+ (Math/abs (- x0 x1))
     (Math/abs (- y0 y1))
     (Math/abs (- z0 z1))))

(defn inputs-in-range [inputs {radius :r pos :p}]
  (filter (fn [{p :p}] (<= (distance p pos) radius)) inputs))

(def puzzle (parsed puzzle-input))

(defn coord-range [inputs idx]
  (let [min-coords (map (fn [{pos :p r :r}] (- (get pos idx) r)) inputs)
        max-coords (map (fn [{pos :p r :r}] (+ (get pos idx) r)) inputs)]
    (vector (apply min min-coords) (apply max max-coords))))

(defn log2 [n] (/ (Math/log n) (Math/log 2)))

(defn power-of-two [n]
  (->> n
       log2
       Math/ceil
       (Math/pow 2)
       int))

(defn cube-center [size start]
  (mapv #(int (Math/floor (+ (/ size 2) %))) start))

(defn cube-distance [{size :size start :start}]
  (distance [0 0 0] (cube-center size start)))

(defn cube-in-range [spheres cube]
  (let [{size :size start :start} cube
        center (cube-center size start)
        diag (distance center start)]
    (count (filter (fn [{r :r p :p}] (<= (distance center p) (+ r diag))) spheres))))

(defn initial-search-cube [inputs]
  (let [ranges (map #(coord-range inputs %) (range 0 3))
        max-length (apply max (mapv (fn [[m n]] (- n m)) ranges))
        size (power-of-two max-length)]
    {:size size
     :start (mapv first ranges)
     :distance (distance [0 0 0] (cube-center size (mapv first ranges)))
     :in-range (count inputs)}))

(defn split-cubes [{size :size start :start} spheres]
  (let [half (/ size 2)
        s (for [x0 (range 0 2) y0 (range 0 2) z0 (range 0 2)]
            (vector (* half x0) (* half y0) (* half z0)))]
    (->> s
         (map #(assoc {} :start (mapv + % start)))
         (map #(assoc % :size half))
         (map #(assoc % :distance (cube-distance %)))
         (map #(assoc % :in-range (cube-in-range spheres %))))))

(defn q-comp [[ir0 d0 s0] [ir1 d1 s1]]
  (if (not (= ir0 ir1)) (> ir0 ir1)
      (if (not (= d0 d1)) (< d0 d1)
          (< s0 s1))))

(defn q-reducer [pqueue cube]
  (let [{ir :in-range d :distance s :size} cube]
    (assoc pqueue cube (vector ir d s))))

(defn search-cubes [spheres]
  (let [initial-cube (initial-search-cube spheres)
        {ir :in-range d :distance s :size} initial-cube]
    (loop [queue (pm/priority-map-by q-comp initial-cube (vector ir d s))]
      (if-let [[cube cube-in-range] (first queue)]
        (if (= (cube :size) 1) cube
            (recur (reduce q-reducer (dissoc queue cube)
                           (split-cubes cube spheres))))))))



; Part 1
#_(count (inputs-in-range puzzle
                          (max-radius puzzle)))
; Part 2
#_(search-cubes puzzle)
