(ns y2025.d1
  (:require [clojure.string :as str]))

(def dummy-input)

(def input (slurp "data/d1.txt"))

(defn parse [s]
  (->> (str/split-lines s)))

(def data (parse input))

