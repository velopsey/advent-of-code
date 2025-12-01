(ns y2025.d1
  (:require [clojure.string :as str]))

(def input (slurp "data/d1.txt"))

(defn parse [s]
  (->> (str/split-lines s)))

(def data (parse input))

;; common

(def start-position 50)

(defn parse-code [code]
  [(if (str/starts-with? code "L") - +)
   (parse-long (subs code 1))])

;; part - 1

(defn rotate [v code]
  (let [[op num] (parse-code code)]
    (mod (op v num) 100)))

(defn rotate-history [initial-pos data]
  (reductions rotate initial-pos data))

(->> (rotate-history start-position data)
     (filter zero?)
     count) ;; answer

;; part - 2

(defn crossed-zero? [op old-pos new-pos]
  (or (zero? new-pos)
      (and (not (zero? old-pos))
           (if (= op +)
             (< new-pos old-pos)
             (> new-pos old-pos)))))

(defn rotate-zero-count [[pos total-count] code]
  (let [[op num] (parse-code code)
        full-rotations (quot num 100)
        new-pos (mod (op pos num) 100)
        boundary-cross (if (crossed-zero? op pos new-pos) 1 0)]
    [new-pos (+ total-count full-rotations boundary-cross)]))

(second (reduce rotate-zero-count [start-position 0] data)) ;; answer
