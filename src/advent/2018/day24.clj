(ns advent.2018.day24
  (:require [clojure.string :as str]))

; system -> groups -> units
; group definition
; - number of units
; - hit points for each unit
; - immune to x, y, z
; - weak to x, y
; - attack damage (number)
; - attack type (radiation, fire, cold, bludgeoning)
; - initiative (number)
; effective power = num units * attack damage

(defn parse-inner [input splitter]
  (let [has-imm (> (count (str/split input splitter)) 1)]
    (if has-imm
      (set (str/split (first (str/split (second (str/split input splitter))
                                        #"[;\)]")) #", "))
      #{})))

(defn immunities [input] (parse-inner input #"immune to "))

(defn weaknesses [input] (parse-inner input #"weak to "))

(defn parse-group [input]
  {:units (read-string input)
   :hp (read-string (second (str/split input #"each with ")))
   :immune (immunities input)
   :weak (weaknesses input)
   :attack (read-string (second (str/split input  #"attack that does ")))
   :attack-type (first (str/split (second
                                   (str/split input #"attack that does \d+ ")) #" "))
   :initiative (read-string (second (str/split input  #"at initiative ")))})

(def test-immune
  '("17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2"
    "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"))

(def test-infection
  '("801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1"
    "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"))

(defn not-in? [coll value]
  (not (some #(= % value) coll)))

(defn effective-power [{units :units attack :attack}]
  (* units attack))

(defn estimate-damage [attacker defender]
  (let [is-immune (contains? (defender :immune) (attacker :attack-type))
        is-weak (contains? (defender :weak) (attacker :attack-type))]
    (cond
      is-immune 0
      is-weak (* 2 (effective-power attacker))
      :else (effective-power attacker))))

(defn find-thing [needle haystack]
  (first (keep-indexed #(when (= %2 needle) %1) haystack)))

(defn most-damaged-defender-idx [attacker defending-groups filtered-ids]
  (let [filtered-ds (->> defending-groups
                         (map-indexed #(and (not-in? filtered-ids %1)
                                            ; also filtering out the groups that have been killed
                                            (not (= 0 (%2 :units)))
                                            ; also filtering defenders with 0 estimated damage
                                            (not (= 0 (estimate-damage attacker %2)))
                                            %2))
                         (filter #(true? (boolean %))))
        defenders (reverse (sort-by (juxt #(estimate-damage attacker %)
                                          effective-power :initiative)
                                    filtered-ds))]
    (cond
      (empty? defenders) nil
      :else (let [d (first defenders)]
              (if (some? d) (find-thing d defending-groups) nil)))))

(defn target-selection [attacking-groups defenders]
  (loop [attackers (reverse (sort-by (juxt effective-power :initiative) attacking-groups))
         result (vec (repeat (count attacking-groups) nil))]
    (if (empty? attackers) result
        (let [attacker (first attackers)
              attacker-idx (find-thing attacker attacking-groups)
              idx (most-damaged-defender-idx attacker defenders result)]
          (recur (rest attackers)
                 (assoc result attacker-idx idx))))))

(defn attacked-result [attacker defender]
  (let [{hp :hp units :units} defender
        damage (estimate-damage attacker defender)
        total-hp (* hp units)
        new-total-hp (- total-hp damage)
        new-units (max (int (Math/ceil (/ new-total-hp hp))) 0)]
    (assoc defender :units new-units)))

(defn map-for-ordering [groups system]
  (->> groups
       (map-indexed (fn [i v] {:idx i
                               :units (v :units)
                               :initiative (v :initiative)
                               :system system}))
       (filter #(> (% :units) 0))
       vec))

(defn attack-order [immune-groups infection-groups]
  (->> (concat (map-for-ordering immune-groups "immune")
               (map-for-ordering infection-groups "infection"))
       (sort-by :initiative >)))

(defn run-attack-iteration [immune-groups infection-groups]
  (let [targets-for-immune (target-selection immune-groups infection-groups)
        targets-for-infection (target-selection infection-groups immune-groups)]
    (loop [remaining (attack-order immune-groups infection-groups)
           immunes immune-groups
           infections infection-groups]
      (cond
        (empty? remaining) (vector immunes infections)
        :else (let [{attacker-idx :idx
                     attacker-init :initiative
                     system :system} (first remaining)
                    targets (if (= system "immune") targets-for-immune targets-for-infection)
                    attackers (if (= system "immune") immunes infections)
                    defenders (if (= system "immune") infections immunes)
                    target-idx (get targets attacker-idx)
                    new-defender (if (some? target-idx)
                                   (attacked-result (get attackers attacker-idx)
                                                    (get defenders target-idx))
                                   nil)
                    new-defenders (if (some? new-defender)
                                    (assoc defenders target-idx new-defender)
                                    defenders)]
                (recur (rest remaining)
                       (if (= system "immune") attackers new-defenders)
                       (if (= system "immune") new-defenders attackers)))))))

(defn units [groups]
  (->> groups
       (map #(% :units))
       (reduce +)))

(defn boost-group [groups boost]
  (mapv #(update % :attack (fn [x] (+ x boost))) groups))

(defn run-iterations [immune-groups infection-groups boost]
  (let [boosted-imms (boost-group immune-groups boost)]
    (loop [i 0 immunes boosted-imms infections infection-groups is-draw false]
      (cond
        is-draw {:system "draw" :units 0}
        (= 0 (units immunes)) {:units (units infections) :system "infection"}
        (= 0 (units infections)) {:units (units immunes) :system "immune"}
        :else (let [[new-imms new-infs] (run-attack-iteration immunes infections)
                    new-is-draw (and (= (units immunes) (units new-imms))
                                     (= (units infections) (units new-infs)))]
                (recur (inc i) new-imms new-infs new-is-draw))))))

(defn find-boost [immunes infections]
  (loop [b 0]
    (let [{winner :system units :units} (run-iterations immunes infections b)]
      (if (= winner "immune") units
          (recur (inc b))))))

(def puzzle-input
  (->> "inputs/2018/day24"
       slurp
       str/split-lines))

(def puzzle-input-immunes
  (-> puzzle-input
      (subvec 1 11)))

(def puzzle-input-infections
  (-> puzzle-input
      (subvec 13)))

; Part 1
#_(run-iterations
   (mapv parse-group puzzle-input-immunes)
   (mapv parse-group puzzle-input-infections)
   0)

; Part 2
#_(find-boost
   (mapv parse-group puzzle-input-immunes)
   (mapv parse-group puzzle-input-infections))
