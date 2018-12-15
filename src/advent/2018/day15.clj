(ns advent.2018.day15
  (:require [clojure.string :as str]))

(def puzzle-input
  (str/split-lines (slurp "inputs/2018/day15")))

(defn parse-line [line line-y]
  "every point is [y, x]"
  (into {} (map-indexed #(assoc {} [line-y %1] %2) line)))

(defn get-keys-for [m char]
  (->> m
       (filter (fn [[k v]] (= v char)))
       (map (fn [[k v]] k))))

(defn parse [lines hp]
  (let [m (into {} (map-indexed #(parse-line %2 %1) lines))
        gs (get-keys-for m \G)
        es (get-keys-for m \E)]
    [(zipmap (keys m) (map #(if (or (= % \G) (= % \E)) \. %) (vals m)))
     (zipmap gs (repeat (count gs) hp))
     (zipmap es (repeat (count es) hp))]))

(defn get-adjacents [[y x]]
  (vector [(dec y) x] [y (dec x)] [y (inc x)] [(inc y) x]))

(defn closest-target-map [m targets others current-yx]
  "returns array of closest cells that are in-range to targets"
  (let [in-range (->> targets
                      keys
                      (mapcat get-adjacents)
                      (filter #(nil? (get targets %)))
                      (filter #(nil? (get others %)))
                      (filter #(= \. (get m %))))]
    (loop [result {}
           new-known-yxs (zipmap in-range (map #(vector %) in-range))]
      (let [new-result (into result new-known-yxs)]
        (cond
          (some? (get new-known-yxs current-yx)) new-result
          (empty? new-known-yxs) new-result
          :else (let [newer-known-yxs (->> new-known-yxs
                                           (map (fn [[k v]] (->> k
                                                                 get-adjacents
                                                                 (filter #(nil? (get targets %)))
                                                                 (filter #(= \. (get m %)))
                                                                 (filter #(nil? (get new-result %)))
                                                                 (filter #(nil? (get others %)))
                                                                 (map #(assoc {} % v))
                                                                 (into {})))))
                      merged (apply merge-with into newer-known-yxs)]
                  (recur new-result merged)))))))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn new-moved-yx [current-yx target-map]
  "moving logic: first pick the closest cell in range of a target, and then
  find best adjacent to reach that cell."
  (if-let [available-targets (get target-map current-yx)]
    (let [target-range (first (sort available-targets))
          adjs (->> current-yx
                    get-adjacents
                    (filter #(some? (get target-map %)))
                    (filter #(in? (get target-map %) target-range)))]
      (if (not (empty? adjs)) (first adjs)))))
        
(defn attacked-result [current-yx targets us attack-power]
  "find the adjacent target with least hp to attack."
  (let [min-hp (->> current-yx
                    get-adjacents
                    (select-keys targets)
                    (apply min-key val)
                    second)
        target-yx (->> current-yx
                       get-adjacents
                       (filter #(= min-hp (get targets %)))
                       first)]
    [(update targets target-yx #(- % attack-power)) us]))

(defn next-ge-move [target-map current-yx current-hp targets us attack-power]
  "can attack if adjacent, or will move towards closest."
  (let [target-ranges (get target-map current-yx)
        is-in-range (= target-ranges (vector current-yx))]
    (if is-in-range 
      (attacked-result current-yx targets us attack-power)
      ; else: try move and then attack
      (let [new-yx (new-moved-yx current-yx target-map)
            new-us (assoc (dissoc us current-yx) new-yx current-hp)
            new-target-ranges (get target-map new-yx)
            is-now-in-range (= new-target-ranges (vector new-yx))]
        (cond
          (nil? new-yx) [targets us] ; can't move
          is-now-in-range (attacked-result new-yx targets new-us attack-power) ; move and attack
          :else [targets new-us] ; just move
          )))))

(defn next-move-of [game-map gs es current-yx elf-attack-power]
  "return new gs and es"
  (let [goblin (get gs current-yx) elf (get es current-yx)]
    (cond
      (some? goblin) (let [elf-map (closest-target-map game-map es (dissoc gs current-yx) current-yx)
                           [es1 gs1] (next-ge-move elf-map current-yx goblin es gs 3)]
                       [gs1 es1])

      (some? elf) (let [goblin-map (closest-target-map game-map gs (dissoc es current-yx) current-yx)
                        [gs1 es1] (next-ge-move goblin-map current-yx elf gs es elf-attack-power)]
                    ; (println goblin-map)
                    [gs1 es1])

      :else [gs es] ; => something got attacked and died
      )))

(defn sum-of-hp [xs]
  (reduce + (vals xs)))

(defn filter-neg-hp [xs]
  (let [ks (filter #(pos? (get xs %)) (keys xs))]
    (select-keys xs ks)))

(defn filter-neg-from-remaining [remaining gs es]
  (vec (filter #(or (and (some? (get gs %)) (pos? (get gs %)))
               (and (some? (get es %)) (pos? (get es %)))) remaining)))

(defn print-game-state [game-map gs es]
  (let [ks (keys game-map)
        [max-y max-x] (last (sort ks))]
    (loop [y 0 x 0]
      (cond
        (> y max-y) nil
        (> x max-x) (do
                      (println)
                      (recur (inc y) 0))
        :else (do
                (print (cond
                         (= \# (game-map [y x])) "#"
                         (some? (gs [y x])) "G"
                         (some? (es [y x])) "E"
                         :else "."))
                (recur y (inc x)))))))

(defn run-game [game-map initial-goblins initial-elves elf-attack-power]
  (loop [rounds-finished 0
         gs initial-goblins
         es initial-elves
         round-remaining (sort (concat (keys gs) (keys es)))
         dead-elves 0]
    (cond
      ; round ends
      (empty? round-remaining) (do
                                ;  (println "Round:" (inc rounds-finished) "\n")
                                ;  (print-game-state game-map gs es)
                                 (recur (inc rounds-finished) gs es
                                        (sort (concat (keys gs) (keys es)))
                                        dead-elves))

      ; NOTE: this break is only valid for part 2
      (> dead-elves 0) ["goblins" dead-elves 0]

      ; combat ends
      (empty? gs) ["elves" dead-elves (* rounds-finished (sum-of-hp es))]
      (empty? es) ["goblins" dead-elves (* rounds-finished (sum-of-hp gs))]

      ; normal play
      :else (let [[gs1 es1] (next-move-of game-map gs es (first round-remaining) elf-attack-power)]
              (recur rounds-finished
                     (filter-neg-hp gs1)
                     (filter-neg-hp es1)
                     (filter-neg-from-remaining (rest round-remaining) gs1 es1)
                     (+ dead-elves (- (count es1) (count (filter-neg-hp es1)))))))))

(defn min-attack-power [game-map gs es]
  (loop [elf-power 4]
    (let [[winner dead-elves outcome] (run-game game-map gs es elf-power)]
      (cond
        (and (= winner "elves") (= 0 dead-elves)) [elf-power outcome]
        :else (recur (inc elf-power))))))

(def test-input
  '("#######"
    "#.G...#"
    "#...EG#"
    "#.#.#G#"
    "#..G#E#"
    "#.....#"
    "#######"))

(def test-input-2
  '("#########"
    "#G......#"
    "#.E.#...#"
    "#..##..G#"
    "#...##..#"
    "#...#...#"
    "#.G...G.#"
    "#.....G.#"
    "#########"))

(def test-input-3
  '("#######"
    "#E..EG#"
    "#.#G.E#"
    "#E.##E#"
    "#G..#.#"
    "#..E#.#"
    "#######"))

; Part 1
#_(let [[m gs es] (parse puzzle-input 200)]
    (run-game m gs es 3)))

; Part 2
#_(let [[m gs es] (parse puzzle-input 200)]
    (min-attack-power m gs es)))
