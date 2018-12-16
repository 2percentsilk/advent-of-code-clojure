(ns advent.2018.day16
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn addr [regs [opcode A B C]]
  (assoc regs C (+ (get regs A) (get regs B))))

(defn addi [regs [opcode A B C]]
  (assoc regs C (+ (get regs A) B)))

(defn mulr [regs [opcode A B C]]
  (assoc regs C (* (get regs A) (get regs B))))

(defn muli [regs [opcode A B C]]
  (assoc regs C (* (get regs A) B)))

(defn banr [regs [opcode A B C]]
  (assoc regs C (bit-and (get regs A) (get regs B))))

(defn bani [regs [opcode A B C]]
  (assoc regs C (bit-and (get regs A) B)))

(defn borr [regs [opcode A B C]]
  (assoc regs C (bit-or (get regs A) (get regs B))))

(defn bori [regs [opcode A B C]]
  (assoc regs C (bit-or (get regs A) B)))

(defn gtir [regs [opcode A B C]]
  (if (> A (get regs B))
    (assoc regs C 1)
    (assoc regs C 0)))

(defn gtri [regs [opcode A B C]]
  (if (> (get regs A) B)
    (assoc regs C 1)
    (assoc regs C 0)))

(defn gtrr [regs [opcode A B C]]
  (if (> (get regs A) (get regs B))
    (assoc regs C 1)
    (assoc regs C 0)))

(defn eqir [regs [opcode A B C]]
  (if (= A (get regs B))
    (assoc regs C 1)
    (assoc regs C 0)))

(defn eqri [regs [opcode A B C]]
  (if (= (get regs A) B)
    (assoc regs C 1)
    (assoc regs C 0)))

(defn eqrr [regs [opcode A B C]]
  (if (= (get regs A) (get regs B))
    (assoc regs C 1)
    (assoc regs C 0)))

(defn setr [regs [opcode A B C]]
  (assoc regs C (get regs A)))

(defn seti [regs [opcode A B C]]
  (assoc regs C A))

(defn opcodes-for-sample [before after instruction]
  (loop [remaining '("addr" "addi" "mulr" "muli" "banr" "bani"
                            "borr" "bori" "gtir" "gtri" "gtrr"
                            "eqir" "eqri" "eqrr" "setr" "seti")
         possible-opcodes #{}]
    (if (empty? remaining) (assoc {} (first instruction) possible-opcodes)
        (let [ins-str (str "(" (first remaining) " " before " " instruction ")")
              result (eval (read-string ins-str))
              is-matched (= after result)]
          (recur (rest remaining)
                 (if is-matched (conj possible-opcodes (first remaining))
                     possible-opcodes))))))

(defn parse-sample [input]
  (let [[b i a] (str/split-lines input)]
    [(read-string (subs b 8))
     (read-string (str "[" i "]"))
     (read-string (subs a 8))]))

(def puzzle-samples
  (drop-last 2 (-> (slurp "inputs/2018/day16")
                   (str/split #"\n\n"))))

(def puzzle-program
  (-> (slurp "inputs/2018/day16")
      (str/split #"\n\n")
      last
      str/split-lines))

(defn opcode-mapping [samples]
  (let [possible-mappings  (->> puzzle-samples
                                (map parse-sample)
                                (map (fn [[b i a]] (opcodes-for-sample b a i)))
                                (apply merge-with set/intersection))]
    (loop [mappings possible-mappings
           result {}]
      (let [singulars (filter (fn [[k v]] (= 1 (count v))) mappings)]
        (cond
          (empty? mappings) result
          (empty? singulars) result
          :else (let [[opcode ins-set] (first singulars)
                      ins (first ins-set)]
                  (recur (into {} (map (fn [[k v]] [k (disj v ins)])
                                       (dissoc mappings opcode)))
                         (assoc result opcode ins))))))))

(defn run-program [instruction-lines opcode-mapping]
  (loop [remaining instruction-lines
         regs [0 0 0 0]]
    (cond
      (empty? remaining) regs
      :else (let [[opcode A B C] (->> (str/split (first remaining) #" ")
                                      (mapv read-string))
                  instruction-name (get opcode-mapping opcode)
                  ins-str (str "(" instruction-name " " regs " " [opcode A B C] ")")
                  result-regs (eval (read-string ins-str))]
              (recur (rest remaining) result-regs)))))

; Part 1
#_(->> puzzle-samples
       (map parse-sample)
       (map (fn [[b i a]] (opcodes-for-sample b a i)))
       (filter (fn [hm] (>= (count (first (vals hm))) 3)))
       count)

; Part 2
#_(run-program puzzle-program
               (opcode-mapping puzzle-samples))
