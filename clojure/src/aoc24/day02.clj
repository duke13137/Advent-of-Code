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

(defn is-safe-with-dampener?
  [report]
  (if (is-safe? report)
    true
    (some (fn [i]
            (is-safe? (concat (take i report) (drop (inc i) report))))
          (range (count report)))))

(defn part-1
  [_]
  (->> input
       (filter is-safe?)
       count
       prn))

(defn part-2
  [_]
  (->> input
       (filter is-safe-with-dampener?)
       count
       prn))

(comment

  (part-1 nil)
  (part-2 nil)

  'comment)