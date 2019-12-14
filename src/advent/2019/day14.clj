(ns advent.2019.day13
  (:require [clojure.string :as str]))

(def test-input
  '("10 ORE => 10 A"
    "1 ORE => 1 B"
    "7 A, 1 B => 1 C"
    "7 A, 1 C => 1 D"
    "7 A, 1 D => 1 E"
    "7 A, 1 E => 1 FUEL"))

(defn ores? [chems]
  (= (keys chems) '("ORE")))

(defn parse [recipes]
  (map #(str/split % #" => ") recipes))

(defn recipe-for [chem recipes]
  (->> recipes
       (filter #(= (get (str/split (get % 1) #" ") 1) chem))
       first))

(defn input-for-one [[recipe-input recipe-output]]
  (->> (str/split recipe-input #", ")
       (map #(str/split % #" "))
       (map #(assoc % 0 (read-string (get % 0))))
       (map #(hash-map (get % 1) (get % 0)))
       (into {})))

(defn mult-values [chems mult]
  (into {} (for [[k v] chems] [k (* mult v)])))

(defn input-chemicals [recipe volume]
  (let [recipe-output (read-string (get (str/split (get recipe 1) #" ")
                                        0))
        recipe-times (Math/ceil (/ volume recipe-output))]
   (mult-values (input-for-one recipe) recipe-times)))

(assert 
 (= (input-chemicals ["7 A, 1 E" "1 FUEL"] 2)
    {"A" 14.0 "E" 2.0}))
(assert 
 (= (input-chemicals ["10 ORE" "10 A"] 2)
    {"ORE" 10.0}))
(assert
 (= (input-chemicals ["10 ORE" "10 A"] 12)
    {"ORE" 20.0}))

(defn breakdown [chemicals recipes]
  (let [k (first (filter #(not= % "ORE")
                         (keys chemicals)))
        _ (println chemicals)
        recipe (recipe-for k recipes)]
    (merge-with + (dissoc chemicals k)
                (input-chemicals recipe (get chemicals k)))))

(assert
 (= (breakdown {"FUEL" 1} (parse test-input))
    {"A" 7.0 "E" 1.0}))
(assert
 (= (breakdown {"A" 11} (parse test-input))
    {"ORE" 20.0}))

(defn get-ores [chemicals recipes]
  (loop [chems-state chemicals]
    (if (ores? chems-state) chems-state
        (recur (breakdown chems-state recipes)))))

(get-ores {"FUEL" 1} (parse test-input))
(get-ores {"E" 1} (parse test-input))
