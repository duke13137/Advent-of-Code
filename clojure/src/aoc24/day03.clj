(ns aoc24.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "aoc24/day03.txt"))
                (str/split-lines)
                (map parse-long)))

(defn part-1
  "Run with bb -x aoc24.day03/part-1"
  [_]
  (->> input
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (apply max)
       prn))

(defn part-2
  "Run with bb -x aoc24.day02/part-2"
  [_]
  (->> input
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (sort-by -)
       (take 3)
       (apply +)
       prn))
