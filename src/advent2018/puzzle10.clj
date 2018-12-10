;; https://adventofcode.com/2018/day/10
(ns advent2018.puzzle10
  (:require [clojure.string :as str]))

(def test-lines
  '("position=< 9,  1> velocity=< 0,  2>"
     "position=< 7,  0> velocity=<-1,  0>"
     "position=< 3, -2> velocity=<-1,  1>"
     "position=< 6, 10> velocity=<-2, -1>"
     "position=< 2, -4> velocity=< 2,  2>"
     "position=<-6, 10> velocity=< 2, -2>"
     "position=< 1,  8> velocity=< 1, -1>"
     "position=< 1,  7> velocity=< 1,  0>"
     "position=<-3, 11> velocity=< 1, -2>"
     "position=< 7,  6> velocity=<-1, -1>"
     "position=<-2,  3> velocity=< 1,  0>"
     "position=<-4,  3> velocity=< 2,  0>"
     "position=<10, -3> velocity=<-1,  1>"
     "position=< 5, 11> velocity=< 1, -2>"
     "position=< 4,  7> velocity=< 0, -1>"
     "position=< 8, -2> velocity=< 0,  1>"
     "position=<15,  0> velocity=<-2,  0>"
     "position=< 1,  6> velocity=< 1,  0>"
     "position=< 8,  9> velocity=< 0, -1>"
     "position=< 3,  3> velocity=<-1,  1>"
     "position=< 0,  5> velocity=< 0, -1>"
     "position=<-2,  2> velocity=< 2,  0>"
     "position=< 5, -2> velocity=< 1,  2>"
     "position=< 1,  4> velocity=< 2,  1>"
     "position=<-2,  7> velocity=< 2, -2>"
     "position=< 3,  6> velocity=<-1, -1>"
     "position=< 5,  0> velocity=< 1,  0>"
     "position=<-6,  0> velocity=< 2,  0>"
     "position=< 5,  9> velocity=< 1, -2>"
     "position=<14,  7> velocity=<-2,  0>"
     "position=<-3,  6> velocity=< 2, -1>"))

(def puzzle-input
  (str/split-lines (slurp "inputs/puzzle10")))

(defn parse-nums [input]
  (vec (map read-string (str/split (str/trim input) #", "))))

(defn parse [input]
  (let [split (drop 1 (re-find #"position=<(.+)> velocity=<(.+)>" input))]
    {:position (parse-nums (first split))
     :velocity (parse-nums (second split))}))

(defn has-line-at-x [positions x]
  "filter the y values at x, see if they increment by 1"
  (let [y-vals (set (map #(get % 1) (filter #(= x (get % 0)) positions)))
        ys (sort y-vals)
        r (map-indexed #(- (- %2 (first ys)) %1) ys)]
    (empty? (filter #(not (= 0 %)) r))))

(def expected-vertical-lines-in-result 3)

(defn check-aligned [positions]
  "return if are >=5 vertical lines"
  (let [x-positions (map #(get % 0) positions)
        f-x (frequencies x-positions)
        candidates-x (map #(first %) (filter #(> (val %) 5) f-x))
        x-with-lines (filter #(has-line-at-x positions %) candidates-x)]
    (>= (count x-with-lines) expected-vertical-lines-in-result)))

(defn print-positions [parsed]
  (let [positions (map #(% :position) parsed)
        pos-map (zipmap positions (repeat (count positions) "#"))
        min-x (apply min (map #(get % 0) positions))
        max-x (apply max (map #(get % 0) positions))
        min-y (apply min (map #(get % 1) positions))
        max-y (apply max (map #(get % 1) positions))
        are-aligned (check-aligned positions)]
    (if are-aligned
      (loop [y min-y result ""]
        (cond
          (<= y max-y) (let [line (loop [x min-x line ""]
                                    (cond
                                      (<= x max-x) (recur (inc x) (str line (get pos-map [x y] " ")))
                                      :else line))]
                         (recur (inc y) (str result "\n" line)))
          :else result)

        ))))

(defn update-position [parsed]
  (update parsed :position
          #(vector (+ (get % 0) (get (parsed :velocity) 0))
                   (+ (get % 1) (get (parsed :velocity) 1)))))

(defn run [limit input-lines]
  (loop [n 1
         ps (map parse input-lines)]
    (cond
      (<= n limit) (let [ups (map update-position ps)
                         result (print-positions ups)]
                     (if (nil? result) (recur (inc n) ups)
                                       (str "Iteration: " n "\n" result)))
      :else nil)))

(comment


  (println (run 10 test-lines))
  ; Iteration: 3
  ;
  ; #   #  ###
  ; #   #   #
  ; #   #   #
  ; #####   #
  ; #   #   #
  ; #   #   #
  ; #   #   #
  ; #   #  ###


  )

(defn -main []
  (println (run 20000
                puzzle-input)))

; Iteration: 10243
;
;   ##    #    #  ######   ####   #####   #    #  ######  ######
;  #  #   #    #  #       #    #  #    #  #   #   #       #
; #    #  #    #  #       #       #    #  #  #    #       #
; #    #  #    #  #       #       #    #  # #     #       #
; #    #  ######  #####   #       #####   ##      #####   #####
; ######  #    #  #       #  ###  #  #    ##      #       #
; #    #  #    #  #       #    #  #   #   # #     #       #
; #    #  #    #  #       #    #  #   #   #  #    #       #
; #    #  #    #  #       #   ##  #    #  #   #   #       #
; #    #  #    #  #        ### #  #    #  #    #  ######  ######
