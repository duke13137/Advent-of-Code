(ns aoc24.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [file]
  (str/trim (slurp (io/resource file))))

(defn execute-mul [line]
  (let [matches (re-seq #"mul\((\d+),(\d+)\)" line)]
    (reduce (fn [sum [_ a b]]
              (+ sum (* (parse-long a) (parse-long b))))
            0
            matches)))

(defn part-1
  "Run with bb -x aoc24.day03/part-1"
  [input]
  (execute-mul input))

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

(def input (parse-input "aoc24/day03.txt"))
