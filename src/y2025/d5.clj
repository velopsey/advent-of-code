(ns y2025.d5
  (:require [clojure.string :as str]))

(def input (slurp "data/d5.txt"))

(defn parse [s]
  (let [[ranges nums] (->> (str/split-lines s)
                           (partition-by empty?)
                           (remove #(= % [""])))]
    [(map #(mapv parse-long (str/split % #"-")) ranges)
     (map parse-long nums)]))

(def data (parse input))

;; part 1

(defn in-range? [num ranges]
  (some (fn [[start end]] (<= start num end)) ranges))

(defn check-nums [[ranges num]]
  (->> num
       (filter #(in-range? % ranges))
       count))

(check-nums data) ;; 690

;; part 2

(defn merge-range [[a b] [_ d]]
  (cond
    (> b d) [a b]
    (<= b d) [a d]))

(defn merge-overlapping [ranges]
  (reduce
   (fn [merged-ranges next-range]
     (if (empty? merged-ranges)
       [next-range]
       (let [last-merged (last merged-ranges)
             cross? (fn [[_ b] [c _]] (>= b c))]
         (if (cross? last-merged next-range)
           (conj (pop merged-ranges) (merge-range last-merged next-range))
           (conj merged-ranges next-range)))))
   []
   (sort-by first ranges)))

(defn count-numbers [ranges]
  (->> ranges
       merge-overlapping
       (map (fn [[start end]] (inc (- end start))))
       (reduce +)))

(count-numbers (first data)) ;; 344323629240733
