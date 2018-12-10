;; https://adventofcode.com/2018/day/4
(ns advent.2018.day4.part1
  (:require [clojure.string :as str]))

(defn parse-guard-id-line [x]
  (drop 1 (re-find #"\[(.+) (.+)\] Guard #(\d+).+" x)))

(defn parse-wake-up-line [x]
  (drop 1 (re-find #"\[(.+) (.+)\] wakes up" x)))

(defn parse-fall-asleep-line [x]
  (drop 1 (re-find #"\[(.+) (.+)\] falls asleep" x)))

(defn insert-sorted [existing new-value]
  (loop [seq existing
         result []]
    (cond
      (empty? seq) (conj result new-value)
      (< (first seq) new-value) (recur (rest seq) (conj result (first seq)))
      :else (apply conj result new-value seq))))

(defn extract-min [x]
  (read-string (last (re-find #"(\d+):0*(\d+)" x))))

(defn extract-hour [x]
  (read-string (first (drop 1 (re-find #"(\d+):(\d+)" x)))))

(defn time-hash-map-merged [acc current]
  (let [key (first (keys current))
        new-value (first (vals current))
        value (get acc key [])]
    (assoc acc key (insert-sorted value new-value))))

(defn string-hash-map-merged [acc current]
  (let [key (first (keys current))
        new-value (first (vals current))
        value (get acc key [])]
    (assoc acc key (conj value new-value))))

(defn add-one-day [date]
  ; we could have a case where the month also needs to be incremented
  ; (for example, input is last day of the month, 2018-01-31) --> ignoring this for now
  (let [re-result (drop 1 (re-find #"(\d+-\d+-)0*(\d+)" date))
        new-date (+ 1 (read-string (last re-result)))
        prefix (if (<= new-date 9) "0" "")]
    (str (first re-result) prefix new-date)))

(defn guard-assigned-date [date time]
  "if time is before midnight (23:30), we need to return the following date"
  (if (> (extract-hour time) 1)
    (add-one-day date)
    date))

(defn hash-map-for-value [regex-result]
  (let [[date time value] regex-result]
    (hash-map (read-string value) (guard-assigned-date date time))))

(defn hash-map-for-time [regex-result]
  (let [[date time] regex-result]
    (hash-map date (extract-min time))))

(defn guard-ids [lines]
  (reduce
    string-hash-map-merged {}
    (map hash-map-for-value (remove empty? (map parse-guard-id-line lines)))))

(defn wake-up-times [lines]
  (reduce
    time-hash-map-merged {}
    (map hash-map-for-time (remove empty? (map parse-wake-up-line lines)))))

(defn fall-asleep-times [lines]
  (reduce
    time-hash-map-merged {}
    (map hash-map-for-time (remove empty? (map parse-fall-asleep-line lines)))))

(def test-guard-lines
  '("[1518-11-01 23:58] Guard #99 begins shift"
    "[1518-11-02 00:45] falls asleep"
    "[1518-11-02 00:40] wakes up"
    "[1518-11-08 23:57] Guard #100 begins shift"
    "[1518-11-02 00:25] falls asleep"
    "[1518-11-02 00:50] wakes up"
    "[1518-11-10 23:57] Guard #100 begins shift"))

(assert (= {99 ["1518-11-02"], 100 ["1518-11-09", "1518-11-11"]} (guard-ids test-guard-lines)))
(assert (= {"1518-11-02" [40 50]} (wake-up-times test-guard-lines)))
(assert (= {"1518-11-02" [25 45]} (fall-asleep-times test-guard-lines)))

(defn sleep-ranges-for-date [date wake-times asleep-times]
  (loop [wake wake-times
         asleep asleep-times
         ranges []]
    (cond
      (empty? asleep) {date ranges}
      :else (recur (rest wake) (rest asleep) (conj ranges [(first asleep) (first wake)])))))

(assert (= {"2018" [[5 24] [30 54]]} (sleep-ranges-for-date "2018" [24 54] [5 30])))

(defn sleep-ranges-for-guards [lines]
  (let [guard-dates (guard-ids lines)
        wake-times (wake-up-times lines)
        asleep-times (fall-asleep-times lines)
        id-keys (keys guard-dates)]
    (into {} (map
      (fn [x]
        (hash-map x (into {} (map (fn [y] (sleep-ranges-for-date y (get wake-times y) (get asleep-times y)))
        (get guard-dates x)))))
      id-keys
    ))))

(assert
  (=
    {99 {"1518-11-02" [[25 40] [45 50]]}, 100 {"1518-11-09" [], "1518-11-11" []}}
    (sleep-ranges-for-guards test-guard-lines)))

(defn sum-over-range [total current]
  (let [[start end] current]
    (+ total (- end start))))

(defn total-minutes-slept [date-ranges]
  (reduce sum-over-range 0 (apply concat (vals date-ranges))))

(defn guard-minutes-slept [guard-ranges]
  (let [id-keys (keys guard-ranges)]
    (into {} (map (fn [x] (hash-map x (total-minutes-slept (get guard-ranges x)))) id-keys))))

(defn range-list [input]
  (let [[start end] input]
    (range start end)))

(defn max-frequency-minute [ranges]
  (let [freqs (frequencies (apply concat (map range-list ranges)))]
    (if
      (empty? freqs)
      [0 0] ; spends 0 times sleeping on zeroth-minute
      (apply max-key val freqs))))

(def test-lines-2
  '("[1518-11-01 00:00] Guard #10 begins shift"
    "[1518-11-01 00:05] falls asleep"
    "[1518-11-01 00:25] wakes up"
    "[1518-11-01 00:30] falls asleep"
    "[1518-11-01 00:55] wakes up"
    "[1518-11-01 23:58] Guard #99 begins shift"
    "[1518-11-02 00:40] falls asleep"
    "[1518-11-02 00:50] wakes up"
    "[1518-11-03 00:05] Guard #10 begins shift"
    "[1518-11-03 00:24] falls asleep"
    "[1518-11-03 00:29] wakes up"
    "[1518-11-04 00:02] Guard #99 begins shift"
    "[1518-11-04 00:36] falls asleep"
    "[1518-11-04 00:46] wakes up"
    "[1518-11-05 00:03] Guard #99 begins shift"
    "[1518-11-05 00:45] falls asleep"
    "[1518-11-05 00:55] wakes up"))

(assert (= {10 50, 99 30} (guard-minutes-slept (sleep-ranges-for-guards test-lines-2))))

(defn max-sleep-guard [lines]
  (let [guard-ranges (sleep-ranges-for-guards lines)
        max-guard-id (key (apply max-key val (guard-minutes-slept guard-ranges)))
        max-guard-ranges (get guard-ranges max-guard-id)
        flattened-ranges (apply concat (vals max-guard-ranges))]
    (* max-guard-id (key (max-frequency-minute flattened-ranges)))))

(assert (= 240 (max-sleep-guard test-lines-2)))

(def parsed-input
  (str/split-lines (slurp "inputs/2018/day4")))

(defn -main [] (println (max-sleep-guard parsed-input)))
