(ns y2025.d8
  (:require [clojure.string :as str]))

(def input (slurp "data/d8.txt"))

(defn parse [s]
  (->> (str/split-lines s)
       (map (comp (partial mapv parse-long)
                  #(str/split % #",")))))

(def data (parse input))

;; common

(defn distance [p q]
  (->> (map - p q)
       (map #(* % %))
       (reduce +)
       Math/sqrt))

(defn all-pairs [[x & xs]]
  (when x
    (concat
      (map (fn [y] [x y (distance x y)]) xs)
      (all-pairs xs))))

;; part 1

(defn group-connected [n pairs]
  (->> pairs
       (sort-by last)
       (take n)
       (reduce
         (fn [m [a b]]
           (let [ga (get m a #{a})
                 gb (get m b #{b})]
             (if (= ga gb)
               m 
               (let [merged (into ga gb)]
                 (reduce #(assoc %1 %2 merged) m merged)))))
         {})
       vals
       set
       ))

(->>  (all-pairs data)
      (group-connected 1000)
      (map count)
      (sort >)
      (take 3)
      (apply *)
      ) ;; 131150

;; part 2

(defn connect-until-one [points pairs]
  (let [total (count points)
        sorted-pairs (sort-by last pairs)]
    (reduce
      (fn [m [a b :as pair]]
        (let [ga (get m a #{a})
              gb (get m b #{b})]
          (cond
            (= ga gb) m
            
            :else
            (let [merged (into ga gb)
                  new-m (reduce #(assoc %1 %2 merged) m merged)]
              (if (= (count merged) total)
                (reduced pair)
                new-m)))))
      {}
      sorted-pairs)))

(->> (all-pairs data)
     (connect-until-one data)
     (take 2)
     (map first)
     (apply *)
     ) ;; → 2497445
