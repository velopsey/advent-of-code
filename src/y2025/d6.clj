(ns y2025.d6
  (:require [clojure.string :as str]))

(def input (slurp "data/d6.txt"))

(defn parse [s]
  (->> (str/split-lines s)))

(def data (parse input))

;; part 1

(defn grid-str->nums [lines]
  (let [nums (->> lines (mapv #(vec (re-seq #"\S+" %))))]
    (conj (mapv #(mapv parse-long %) (butlast nums))
          (mapv {"*" * "+" +} (last nums)))))

(defn sum-all-cols [grid]
  (let [ops (last grid)
        nums (pop grid)]
    (->> ops
         (map-indexed
          (fn [idx op]
            (->> nums
                 (map #(nth % idx))
                 (reduce op))))
         (reduce +))))

(-> data
    grid-str->nums
    sum-all-cols) ;; 3525371263915

;; part 2

;; col 0: '1',' ',' ','*' → not empty → num col
;; col 1: '2','4',' ',' ' → num col
;; col 3: ' ',' ',' ',' ' → empty → separator
;; col 6: '8',' ',' ',' ' → num col
;; col 7: ' ',' ',' ',' ' → separator
;; col 9: '5','3','2','*' → num col

(defn parse-ops [op-row]
  (->> op-row
       (map str)
       (map str/trim)
       (remove empty?)
       (mapv {"+" + "*" *})))

(defn get-column [grid idx]
  (map #(nth % idx) grid))

(defn chars->number [chars]
  (->> chars
       (remove #{\ })
       (apply str)
       parse-long))

(defn split-into-groups [columns]
  (->> columns
       (partition-by #(every? #{\ } %))
       (remove #(every? #{\ } (first %)))))

(defn count-сephalopod-nums [grid]
  (let [ops (parse-ops (last grid))
        nums (pop grid)]
    (->> (range (count (first nums)))
         (map #(get-column nums %))
         split-into-groups
         (map #(map chars->number %))
         (map-indexed (fn [idx group]
                        (reduce (nth ops idx) group)))
         (reduce +) 
         )))

(count-сephalopod-nums data) ;; 6846480843636
