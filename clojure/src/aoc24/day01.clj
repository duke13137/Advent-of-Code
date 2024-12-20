(ns aoc24.day01
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(def input (->> (slurp (io/resource "aoc24/day01.txt"))
                (str/split-lines)))

(defn part-1
  [_]
  (->> (->> (slurp (io/resource "aoc24/day01.txt"))
             (str/split-lines)
             (map #(str/split % #"\s+"))
             (apply map vector)
             (map #(map parse-long %)))
       (map sort)
       (apply map (fn [x y] (abs (- x y))))
       (apply +)
       prn))

(defn is-safe? [report]
  (if (< (count report) 2)
    true
    (let [diffs (map - (rest report) report)]
      (every? #(>= % 0) diffs))))

(defn can-be-safe? [report]
  (or (is-safe? report)
      (some #(is-safe? (vec (concat (subvec report 0 %) (subvec report (inc %))))) (range (count report)))))

(defn part-2
  [input]
  (->> input
       (map #(str/split % #"\s+"))
       (map #(map parse-long %))
       (filter can-be-safe?)
       count
       prn))

(part-1 nil)
(part-2 input)
