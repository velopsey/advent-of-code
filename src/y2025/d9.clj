(ns y2025.d9
  (:require [clojure.string :as str]))

(def input (slurp "data/d9.txt"))

(defn parse [s]
  (->> (str/split-lines s)
       (mapv #(mapv parse-long (str/split % #",")))))

(def data (parse input))

;; common

(defn area [[x1 y1] [x2 y2]]
  (* (inc (Math/abs (- x2 x1)))
     (inc (Math/abs (- y2 y1)))))

(defn all-pairs [tiles]
  (for [i (range (count tiles))
        j (range (inc i) (count tiles))
        :let [p1 (tiles i)
              p2 (tiles j)]]
    [p1 p2 (area p1 p2)]))

;; part 1

(->> data
     all-pairs
     (apply max-key last)
     last)

;; part 2

(defn point-in-polygon? [[px py] polygon]
  (let [n (count polygon)]
    (loop [inside false, i 0]
      (if (>= i n)
        inside
        (let [[x1 y1] (nth polygon i)
              [x2 y2] (nth polygon (mod (inc i) n))
              intersect? (and (or (and (<= y1 py) (> y2 py))
                                  (and (<= y2 py) (> y1 py)))
                              (< px (+ x1 (/ (* (- x2 x1) (- py y1))
                                             (- y2 y1)))))]
          (recur (if intersect? (not inside) inside)
                 (inc i)))))))

(defn build-edges [red-tiles]
  (let [pairs (partition 2 1 (concat red-tiles [(first red-tiles)]))]
    (group-by
      (fn [[[x1 y1] [x2 y2]]]
        (cond
          (= x1 x2) :vertical
          (= y1 y2) :horizontal
          :else :diagonal))
      pairs)))

(defn rect-intersects-edge? [[x1 y1] [x2 y2] [[ex1 ey1] [ex2 ey2]]]
  (let [rx-min (min x1 x2), rx-max (max x1 x2)
        ry-min (min y1 y2), ry-max (max y1 y2)
        ex-min (min ex1 ex2), ex-max (max ex1 ex2)
        ey-min (min ey1 ey2), ey-max (max ey1 ey2)]
    (cond
      (= ey1 ey2)
      (and (< ry-min ey1 ry-max)         
           (< ex-min rx-max)                
           (< rx-min ex-max))
      
      (= ex1 ex2)
      (and (< rx-min ex1 rx-max)      
           (< ey-min ry-max)          
           (< ry-min ey-max))
      
      :else false)))

(defn valid-rect-fast? [red-tiles edges p1 p2]
  (let [center [(/ (+ (first p1) (first p2)) 2.0)
                (/ (+ (second p1) (second p2)) 2.0)]
        inside? (point-in-polygon? center red-tiles)]
    (and inside?
         (not-any? #(rect-intersects-edge? p1 p2 %)
                   (concat (:horizontal edges) (:vertical edges))))))

(defn solve [red-tiles]
  (let [edges (build-edges red-tiles)]
    (->> (all-pairs red-tiles)
         (sort-by #(- (nth % 2)))        
         (some (fn [[p1 p2 a]]
                 (when (valid-rect-fast? red-tiles edges p1 p2)
                   a))))))

(solve data)
