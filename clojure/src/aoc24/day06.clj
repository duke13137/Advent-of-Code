(ns aoc24.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        grid (mapv vec lines)
        guard-pos (first (for [y (range (count grid))
                                 x (range (count (grid y)))
                                 :when (= \^ (get-in grid [y x]))]
                             [y x]))]
    [grid guard-pos :up]))

(defn part-1
  "Run with bb -x aoc24.day06/part-1"
  [input]
  (let [[map initial-position initial-direction] (parse-input input)]
    (prn map initial-position initial-direction)))

;; (defn part-2
;;   "Run with bb -x aoc24.day02/part-2"
;;   [_]
;;   (->> input
;;        (partition-by nil?)
;;        (take-nth 2)
;;        (map #(apply + %))
;;        (sort-by -)
;;        (take 3)
;;        (apply +)
;;        prn))

(def input (str/split-lines (slurp (io/resource "aoc24/day06.txt"))))

;; (def example (str/split-lines "...")) ; Example input from the problem

;; (part-1 example)
;; (part-2 example)

(part-1 input)
;; (part-2 input)
