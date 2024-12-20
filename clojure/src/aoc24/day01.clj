(ns aoc24.day01
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(def input (->> (slurp (io/resource "aoc24/day01.txt"))
                (str/split-lines)
                (map #(str/split % #"\s+"))
                (apply map vector)
                (map #(map parse-long %))))

(defn part-1
  [_]
  (->> input
       (map sort)
       (apply map (fn [x y] (abs (- x y))))
       (apply +)
       prn))

;; // read <part-2> in day01.md and implement part-2 AI!
(defn part-2
  [_]
  (prn "Not implemented yet!"))
