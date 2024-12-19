(ns aoc24.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "aoc24/day02.txt"))
                (str/split-lines)
                (map #(str/split % #" "))
                (map #(map parse-long %))))

(defn is-safe?
  [report]
  (let [diffs (map - (rest report) report)]
    (and (or (every? #(>= % 0) diffs)
             (every? #(<= % 0) diffs))
         (every? #(<= 1 (Math/abs %) 3) diffs))))

(defn part-1
  [_]
  (->> input
       (filter is-safe?)
       count
       prn))

(defn part-2
  [_]
  (prn "Not implemented yet!"))