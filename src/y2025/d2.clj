(ns y2025.d2
  (:require [clojure.string :as str]
            [clojure.math :as m]))

(def input (slurp "data/d2.txt"))

(defn parse [s]
  (->> (str/split s #",")))


(def data (parse input))

;; part 1 

(defn digit-count [n]
  (count (str n)))

(defn next-boundary [n]
  (long (m/pow 10 (digit-count n))))

(defn split-if-needed [range-str]
  (let [[from to] (map parse-long (str/split range-str #"-"))]
    (if (= (digit-count from) (digit-count to))
      [[from to]]
      (let [boundary (next-boundary from)]
        [[from (dec boundary)]
         [boundary to]]))))

(defn sum-chunk [[from to]]
  (let [len (digit-count from)]
    (if (odd? len)
      0
      (let [k (quot len 2)
            M (inc (long (m/pow 10 k)))
            min-val (long (m/ceil (/ from M)))
            max-val (long (m/floor (/ to M)))]
        (if (<= min-val max-val)
          (quot (* M (+ min-val max-val) (inc (- max-val min-val))) 2)
          0)))))

(->> data
     (mapcat split-if-needed)
     (map sum-chunk)
     (reduce +))

;; part 2

(defn is-repeating-pattern? [s]
  (let [len (count s)]
    (->> (range 1 (inc (quot len 2)))
         (filter #(zero? (mod len %))) 
         (some (fn [pattern-len]
                 (let [pattern (subs s 0 pattern-len)
                       repeated (apply str (repeat (quot len pattern-len) pattern))]
                   (= s repeated)))))))

(defn invalid? [n]
  (is-repeating-pattern? (str n)))

(defn sum-range [range-str]
  (let [[from to] (map parse-long (str/split range-str #"-"))]
    (->> (range from (inc to))
         (filter invalid?)
         (reduce +))))

(->> data
     (map sum-range)
     (reduce +))
