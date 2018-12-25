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
  (loop [remaining inputs
         in-range 0]
    (if (empty? remaining) in-range
        (let [current (get (first remaining) :p)
              d (distance pos current)
              new-in-range (if (<= d radius) (inc in-range) in-range)]
          (recur (rest remaining) new-in-range)))))

(def puzzle (parsed puzzle-input))

(defn events-for-input [input]
  (->> input
       (mapcat (fn [{r :r p :p}] (vector {:t "enter"
                                          :x (- (p 0) r)
                                          :s {:r r :p p}}
                                         {:t "exit"
                                          :x (+ (p 0) r)
                                          :s {:r r :p p}})))
       (sort-by (juxt :x :t))))

(defn has-intersection [{r0 :r p0 :p} {r1 :r p1 :p}]
  (<= (distance p0 p1) (+ r0 r1)))

(defn update-current-intersections [current-intersections current-intersecting new-s]
  (loop [all-ins (keys current-intersections)
         ins-map (assoc current-intersections (set (vector new-s)) 1)]
    (cond
      (empty? all-ins) ins-map
      :else (let [current (first all-ins)
                  intersects-all (set/subset? current current-intersecting)
                  new-ins-map (if intersects-all
                                (assoc ins-map (conj current new-s) (inc (count current)))
                                ins-map)]
              (recur (rest all-ins) new-ins-map)))))

(defn find-best [current-intersections {best-size :s best-elements :e}]
  (let [[max-inter max-size] (first current-intersections)
        is-better (> max-size best-size)]
    (if is-better
      {:s max-size :e max-inter}
      {:s best-size :e best-elements})))

(defn remove-ins-with [current-intersections old-s]
  (loop [all-ins (keys current-intersections)
         ins-map current-intersections]
    (if (empty? all-ins) ins-map
        (recur (rest all-ins)
               (if (contains? (first all-ins) old-s)
                 (dissoc ins-map (first all-ins)) ins-map)))))

(defn sweep-input [input]
  (loop [events (events-for-input input)
         current-entered #{}
         best-so-far {:s 0 :e #{}}]
    (println (first events))
    (if (empty? events) best-so-far
        (let [current (first events)]
          (condp = (current :t)
            "enter" (let [new-s (current :s)
                          intersects-with-best (= (best-so-far :s)
                                                  (count (filter #(has-intersection new-s %) (best-so-far :e))))
                          current-ins (conj (set (filter #(has-intersection new-s %) current-entered))
                                            new-s)
                          new-best (if intersects-with-best
                                     {:s (inc (best-so-far :s)) :e (conj (best-so-far :e) new-s)}
                                     (if (> (count current-ins) (best-so-far :s)) {:s (count current-ins) :e current-ins}
                                         best-so-far))]
                      (recur (rest events)
                             (conj current-entered new-s)
                             new-best))

            "exit" (recur (rest events)
                          (disj current-entered (current :s))
                          best-so-far))))))

(defn gen-points [{r :r [x y z] :p}]
  (let [xmin (- x r) xmax (inc (+ x r))
        ymin (- y r) ymax (inc (+ y r))
        zmin (- z r) zmax (inc (+ z r))]
    (for [x0 (range xmin xmax)
          y0 (range ymin ymax)
          z0 (range zmin zmax)]
      (vector x0 y0 z0))))

(defn is-in-all-spheres [spheres pos]
  (->> spheres
       (filter (fn [{r :r p :p}] (<= (distance p pos) r)))
       count
       (= (count spheres))))

(defn intersection-pts [{spheres :e}]
  (let [smallest-sphere (first (sort-by :r spheres))
        _ (println smallest-sphere)
        _ (println "min x" (apply min (map #(- (get (% :p) 0) (% :r)) spheres)))
        _ (println "max x" (apply min (map #(+ (get (% :p) 0) (% :r)) spheres)))
        ; pts (gen-points smallest-sphere)
        ; _ (println (count pts))
        ]
    (count spheres)))

#_(time (intersection-pts (sweep-input (parsed test-input-2))))

; Part 1
#_(inputs-in-range puzzle
                   (max-radius puzzle))

; Part 2
(defn -main [] (println (intersection-pts (sweep-input (parsed puzzle-input)))))
