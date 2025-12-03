(ns y2025.d3
  (:require [clojure.string :as str]))

(def input (slurp "data/d3.txt"))

(defn parse [s]
  (->> (str/split-lines s)))

(def data (parse input))

(def dummy-data ["987654321111111"
                 "811111111111119"
                 "234234234234278"
                 "818181911112111"])

;; common

(defn char->digit [c]
  (- (int c) (int \0)))

(defn max-n-greedy [s n]
  (let [digits (map char->digit s)]
    (loop [pos 0
           result []]
      
      (if (= (count result) n)
        (apply str result)
        
        (let [remaining (drop pos digits)
              need (- n (count result))
              available (- (count remaining) need -1)
              window (take available remaining)
              best (apply max window)
              skip (.indexOf (vec window) best)]
          
          (recur (+ pos skip 1)
                 (conj result best)))))))

(max-n-greedy "818181911112111" 2)
(max-n-greedy "811111111111119" 2)

;; part 1

(->> data
     (map #(max-n-greedy % 2))
     (map parse-long)
     (reduce + 0)
     ) ;; 17107

;; part 2

(->> data
     (map #(max-n-greedy % 12))
     (map parse-long)
     (reduce + 0)
     ) ;; 169349762274117