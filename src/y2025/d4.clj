(ns y2025.d4
  (:require [clojure.string :as str]))

(def input (slurp "data/d4.txt"))

(defn parse [s]
  (->> (str/split-lines s)
       (mapv vec)
       ))

(def data (parse input))

;; common

(def directions-8
  [[-1 -1] [-1 0] [-1 1]
   [0 -1]          [0 1]
   [1 -1]  [1 0]  [1 1]])

(defn get-neighbors [grid [r c]]
  (let [height (count grid)
        width (count (first grid))]
    (->> directions-8
         (map (fn [[dr dc]] [(+ r dr) (+ c dc)]))
         (filter (fn [[nr nc]]
                   (and (>= nr 0) (< nr height)
                        (>= nc 0) (< nc width))))
         (map (fn [pos] (get-in grid pos)))
         (remove #(= % \.))
         )))

(defn analyze-grid [grid]
  (for [[r row] (map-indexed vector grid)
        [c cell] (map-indexed vector row)]
    {:pos [r c]
     :val cell
     :neighbors (get-neighbors grid [r c])}))

;; part - 1

(->> (analyze-grid data)
     (filter #(= (:val %) \@))
     (filter #(< (count (:neighbors %)) 4))
      count
     ) ;; 1540

;; part - 2 

(defn remove-candidates [grid candidates]
  (reduce (fn [g candidate] (assoc-in g (:pos candidate) \.)) grid candidates))

(loop [grid data
       removed 0]
  (let [candidates (->> (analyze-grid grid)
                        (filter #(= (:val %) \@))
                        (filter #(< (count (:neighbors %)) 4)))]
    (if (empty? candidates)
      removed  
      (recur (remove-candidates grid candidates)
             (+ removed (count candidates)))))) ;; 8972