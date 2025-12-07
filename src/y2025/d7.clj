(ns y2025.d7
  (:require [clojure.string :as str]))

(def input (slurp "data/d7.txt"))

(defn parse [s]
  (->> (str/split-lines s)))
       ;; parse here

(def data (parse input))

;; common 

(defn find-start [row]
  (first (keep-indexed #(when (= %2 \S) %1) row)))

;; part 1

(defn count-splits [rows]
  (reduce
    (fn [{:keys [active splits]} row]
      (let [hats (set (keep-indexed #(when (= %2 \^) %1) row))
            split-count (count (filter hats active))
            new-active (into #{}
                             (mapcat (fn [idx]
                                       (if (hats idx)
                                         [(dec idx) (inc idx)]
                                         [idx])))
                             active)]
        {:active new-active
         :splits (+ splits split-count)}))
    {:active #{(find-start (first rows))} :splits 0}
    rows))

(->> data
     (count-splits)
     :splits) ;; 1640


;; part 2

(defn count-paths [rows]
  (reduce
   (fn [path-counts row]
     (let [hats (set (keep-indexed #(when (= %2 \^) %1) row))]
       (reduce-kv
        (fn [acc idx cnt]
          (if (hats idx)
            (-> acc
                (update (dec idx) (fnil + 0) cnt)
                (update (inc idx) (fnil + 0) cnt))
            (update acc idx (fnil + 0) cnt)))
        {}
        path-counts)))
   {(find-start (first rows)) 1}
   rows))

(->> data
     count-paths
     vals
     (reduce +)
     ) ;; 40999072541589
