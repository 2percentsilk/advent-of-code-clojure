(ns advent.2018.day17
  (:require [clojure.string :as str]))

(defn parse-range [input]
  (mapv read-string (str/split input #"\.\.")))

(defn parse-line [input]
  (let [dir (if (= \x (first input)) "vertical" "horizontal")
        f (read-string (subs input 2))
        s (parse-range (last (str/split input #"=")))]
    {:dir dir
     :x (if (= dir "vertical") f s)
     :y (if (= dir "vertical") s f)
     :type "clay"}))

(defn in-range? [v [a b]]
  (<= a v b))

(def puzzle-input-lines
  (str/split-lines (slurp "inputs/2018/day17")))

(def test-input-lines
  '("x=495, y=2..7"
    "y=7, x=495..501"
    "x=501, y=3..7"
    "x=498, y=2..4"
    "x=506, y=1..2"
    "x=498, y=10..13"
    "x=504, y=10..13"
    "y=13, x=498..504"))

(defn get-max-y [parsed-lines]
  (let [y-max-h (->> parsed-lines
                     (filter #(= "horizontal" (% :dir)))
                     (map #(% :y))
                     (apply max))
        y-max-v (->> parsed-lines
                     (filter #(= "vertical" (% :dir)))
                     (map #(second (% :y)))
                     (apply max))]
    (max y-max-h y-max-v)))

(defn get-min-y [parsed-lines]
  (let [y-min-h (->> parsed-lines
                     (filter #(= "horizontal" (% :dir)))
                     (map #(% :y))
                     (apply min))
        y-min-v (->> parsed-lines
                     (filter #(= "vertical" (% :dir)))
                     (map #(second (% :y)))
                     (apply min))]
    (min y-min-h y-min-v)))

(defn parsed-input [lines]
  "parses input lines, adds horizontal lines for vertical tops
   and for the last y."
  (let [parsed-lines (mapv parse-line lines)
        vertical-tops (->> parsed-lines
                           (filter #(= "vertical" (% :dir)))
                           (map #(vector (% :x) (first (% :y))))
                           (mapv (fn [[x y]] {:dir "horizontal"
                                              :x [x x]
                                              :y y
                                              :type "clay-vertical-top"})))
        y-max (inc (get-max-y parsed-lines))
        end-line [{:dir "horizontal" :y y-max :type "clay-end"}]]
    (reduce into [parsed-lines vertical-tops end-line])))

(defn first-horizontal-line [[startx starty] lines]
  (->> lines
       (filter #(= "horizontal" (% :dir)))
       (filter #(>= (% :y) starty))
       (filter #(or (nil? (get % :x)) (in-range? startx (% :x))))
       (sort-by :y)
       first))

(defn add-vertical-water [water-lines lines x [y-min y-max]]
  (conj water-lines {:type "water"
                     :dir "vertical"
                     :x x
                     :y [y-min y-max]}))

(defn add-horizontal-water [water-lines lines y [x-min x-max]]
  (let [overlapping (->> water-lines
                         (filter #(= "horizontal" (% :dir)))
                         (filter #(= "water" (% :type)))
                         (filter #(= y (% :y)))
                         (filter #(>= (- (second (% :x)) x-min) -1))
                         (filter #(>= (- x-max (first (% :x))) -1))
                         first)]
    (if (some? overlapping) (mapv
                             #(if (and (= (% :x) (overlapping :x))
                                       (= (% :y) (overlapping :y)))
                                (update % :x (fn [[x0 x1]] [(min x-min x0)
                                                            (max x-max x1)]))
                                %)
                             water-lines)
        ; add this line
        (conj water-lines {:type "water"
                           :dir "horizontal"
                           :x [x-min x-max]
                           :y y}))))

(defn vertical-bounds [lines y x]
  "returns left and right bounds at y x"
  (let [verticals (->> lines
                       (filter #(= "vertical" (% :dir)))
                       (filter #(in-range? y (% :y))))
        left (->> verticals
                  (filter #(>= x (% :x)))
                  (sort-by :x)
                  reverse
                  first)
        right (->> verticals
                   (filter #(<= x (% :x)))
                   (sort-by :x)
                   first)]
    [left right]))

(defn water-points [water-lines]
  (loop [points #{}
         lines water-lines]
    (if (empty? lines) points
        (let [l (first lines)
              {t :dir x :x y :y} l]
          (cond
            (= "vertical" t) (let [[y0 y1] y
                                   ps (mapv #(vector x %) (range y0 (inc y1)))]
                               (recur (into points ps) (rest lines)))
            (= "horizontal" t) (let [[x0 x1] x
                                     ps (mapv #(vector % y) (range x0 (inc x1)))]

                                 (recur (into points ps) (rest lines))))))))

(defn bounded-water-points [water-lines lines]
  (loop [points #{}
         wlines water-lines]
    (cond
      (empty? wlines) points

      (= "horizontal" ((first wlines) :dir)) (let [l (first wlines)
                                                   {[x0 x1] :x y :y} l
                                                   lbound (->> lines
                                                               (filter #(= "vertical" (% :dir)))
                                                               (filter #(= (dec x0) (% :x)))
                                                               (filter #(in-range? y (% :y)))
                                                               first)
                                                   rbound (->> lines
                                                               (filter #(= "vertical" (% :dir)))
                                                               (filter #(= (inc x1) (% :x)))
                                                               (filter #(in-range? y (% :y)))
                                                               first)
                                                   ps (mapv #(vector % y) (range x0 (inc x1)))]
                                               (if (and (some? lbound) (some? rbound))
                                                 (recur (into points ps) (rest wlines))
                                                 (recur points (rest wlines))))

      :else (recur points (rest wlines)))))

(defn add-to-starts [starts water-lines xy]
  (if (not (contains? (water-points water-lines) xy))
    (conj starts xy)
    starts))

(defn fill-once [[water-x water-y] limit lines]
  (loop [i 0
         starts (conj '() (vector water-x water-y))
         water-lines []]
    (if (or (> i limit) (empty? starts)) water-lines
        (let [[startx starty] (first starts)
              first-horz (first-horizontal-line [startx starty] (into lines water-lines))
              {type :type y :y [x-min x-max] :x} first-horz
              is-unbounded (->> lines
                                (filter #(and (= "clay" (% :type))
                                              (= "vertical" (% :dir))
                                              (in-range? y (get % :y))
                                              (or (= (dec x-min) (% :x)) (= (inc x-max) (% :x)))))
                                count
                                (= 2)
                                not)
              min-y (get-min-y lines)]
          (cond
            ; we're at the end of the clay definitions
            (= type "clay-end") (recur (inc i) (rest starts)
                                       (add-vertical-water water-lines lines startx
                                                           [(max starty min-y) (dec y)]))

            ; can pop this start since reservoir is full
            (and is-unbounded (= type "water")) (recur (inc i) (rest starts)
                                                       (add-vertical-water water-lines lines startx
                                                                           [(max starty min-y) y]))

            (= y starty) (recur (inc i) (rest starts)
                                (add-vertical-water water-lines lines startx
                                                    [(max starty min-y) y]))

            ; there are 4 types of horizontal lines: bounded vs unbounded
            ; these bounds define whether water will overflow or stay as a line
            ; |____|   |____   ____|    _____
            :else (let [[left-bound right-bound] (vertical-bounds lines (dec y) startx)
                        is-left-bounded (and (some? left-bound) (<= (- x-min (left-bound :x)) 1))
                        is-right-bounded (and (some? right-bound) (<= (- (right-bound :x) x-max) 1))
                        is-unbounded (or (not is-left-bounded) (not is-right-bounded))]
                    (cond
                      (and is-left-bounded is-right-bounded) (recur (inc i) starts
                                                                    (add-horizontal-water water-lines lines (dec y)
                                                                                          [(inc (left-bound :x)) (dec (right-bound :x))]))

                      is-left-bounded (recur (inc i)
                                             (add-to-starts starts water-lines [(+ x-max 1) (dec y)])
                                             (add-horizontal-water water-lines lines (dec y) [(inc (left-bound :x)) x-max]))

                      is-right-bounded (recur (inc i)
                                              (add-to-starts starts water-lines [(- x-min 1) (dec y)])
                                              (add-horizontal-water water-lines lines (dec y) [x-min (dec (right-bound :x))]))

                      :else (recur (inc i)
                                   (add-to-starts (add-to-starts starts water-lines [(+ x-max 1) (dec y)])
                                                  water-lines [(- x-min 1) (dec y)])
                                   (add-horizontal-water water-lines lines (dec y) [x-min x-max])))))))))

; Part 1
#_(->> puzzle-input-lines
       parsed-input
       (fill-once [500 0] 10000)
       water-points
       count)

; Part 2
#_(let [p (parsed-input puzzle-input-lines)
        wlines (fill-once [500 0] 10000 p)]
    (count (bounded-water-points wlines p)))