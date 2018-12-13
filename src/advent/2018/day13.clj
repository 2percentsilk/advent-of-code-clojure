(ns advent.2018.day13
  (:require [clojure.string :as str]))

(def test-input
  '("/->-\\        "
    "|   |  /----\\"
    "| /-+--+-\\  |"
    "| | |  | v  |"
    "\\-+-/  \\-+--/"
    "  \\------/   "))

(def test-input-2
  '("/>-<\\  "
    "|   |  "
    "| /<+-\\"
    "| | | v"
    "\\>+</ |"
    "  |   ^"
    "  \\<->/"))

(def puzzle-input (str/split-lines (slurp "inputs/2018/day13")))

(def carts #{"^" "v" "<" ">"})

(def carts-replacements {"^" "|"
                         "v" "|"
                         "<" "-"
                         ">" "-"})

(defn parse-line [input line-id]
  (zipmap (map #(vector % line-id) (range 0 (.length input)))
          (str/split input #"")))

(defn parse [input-lines]
  (into {} (map-indexed #(parse-line %2 %1) input-lines)))

(defn parse-carts [paths-input]
  "returns paths and carts. carts map has value (>/^...) and num intersections"
  (let [carts-map (into {} (filter (fn [[x y]] (carts y)) paths-input))
        replacements (into {} (map (fn [[x y]] [x (carts-replacements y)]) carts-map))
        with-intersections (into {} (map (fn [[x y]] [x [y 1]]) carts-map))]
    [(merge paths-input replacements) with-intersections]))

(defn new-coords-for-cart [cart-xy cart-val]
  (let [[x y] cart-xy]
    (condp = cart-val
      ">" [(inc x) y]
      "<" [(dec x) y]
      "^" [x (dec y)]
      "v" [x (inc y)])))

(defn rotate-left [cart-val]
  (condp = cart-val
    ">" "^"
    "^" "<"
    "<" "v"
    "v" ">"))

(defn rotate-right [cart-val]
  (condp = cart-val
    ">" "v"
    "v" "<"
    "<" "^"
    "^" ">"))

(defn intersection [cart-val nth-intersection]
  (condp = (mod nth-intersection 3)
    1 (rotate-left cart-val)
    2 cart-val
    0 (rotate-right cart-val)))

(defn handle-backslash [cart-val]
  (condp = cart-val
    ">" (rotate-right cart-val)
    "^" (rotate-left cart-val)
    "<" (rotate-right cart-val)
    "v" (rotate-left cart-val)))

(defn handle-slash [cart-val]
  (condp = cart-val
    ">" (rotate-left cart-val)
    "^" (rotate-right cart-val)
    "<" (rotate-left cart-val)
    "v" (rotate-right cart-val)))

(defn next-tick-for-cart [paths cart-xy cart-map-val]
  "returns [x y] and direction point for a cart"
  (let [[cart-val nth-intersection] cart-map-val
        new-cart-xy (new-coords-for-cart cart-xy cart-val)
        path-val (get paths new-cart-xy)]
    (condp = path-val
      "-" [new-cart-xy [cart-val nth-intersection]]
      "|" [new-cart-xy [cart-val nth-intersection]]
      "+" [new-cart-xy [(intersection cart-val nth-intersection) (inc nth-intersection)]]
      "\\" [new-cart-xy [(handle-backslash cart-val) nth-intersection]]
      "/" [new-cart-xy [(handle-slash cart-val) nth-intersection]])))

(defn next-tick [paths carts]
  "returns on the first collision"
  (loop [carts-ks (sort (keys carts))
         new-carts {}]
    (if-let [current-cart (first carts-ks)]

      (let [[new-xy new-value] (next-tick-for-cart paths current-cart (carts current-cart))
            existing-dup (get carts new-xy)
            new-dup (get new-carts new-xy)
            has-dup (or (some? existing-dup) (some? new-dup))]
        (if has-dup new-xy
            (recur (drop 1 carts-ks) (assoc new-carts new-xy new-value))))

      ; else: no carts are left, which means we call the next tick
      (next-tick paths new-carts))))

(defn sort-inverted [carts]
  (map #(vec (reverse %)) (sort (map #(vec (reverse %)) (keys carts)))))

(defn next-tick-part-2 [paths input-carts]
  "returns after final tick with only one cart remaining (collisions lead to removing carts)"
  (loop [carts input-carts
         carts-in-this-tick {}]
    (cond
      ; only one left at the start of the tick -> end execution
      (and (<= (count carts) 1) (empty? carts-in-this-tick)) (next-tick-for-cart paths
                                                                                 (key (first carts))
                                                                                 (val (first carts)))
      ; tick completed
      (empty? carts) (recur carts-in-this-tick {})
      :else (let [xy (first (sort-inverted carts))
                  updated-carts (dissoc carts xy)
                  [new-xy new-value] (next-tick-for-cart paths xy (carts xy))
                  same-xy-cart (get carts new-xy)
                  same-xy-new-cart (get carts-in-this-tick new-xy)]
              (cond
                (some? same-xy-cart) (recur (dissoc updated-carts new-xy) carts-in-this-tick)
                (some? same-xy-new-cart) (recur updated-carts (dissoc carts-in-this-tick new-xy))
                :else (recur updated-carts (assoc carts-in-this-tick new-xy new-value)))))))

; Part 1
#_(let [[p c] (parse-carts (parse puzzle-input))]
    (next-tick p c))

; Part 2
#_(let [[p c] (parse-carts (parse puzzle-input))]
    (next-tick-part-2 p c))
