(ns aoc24.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "aoc24/day04.txt"))
                (str/split-lines)))

(defn count-occurrences [grid]
  (let [rows (count grid)
        cols (count (first grid))
        word "XMAS"
        word-len (count word)]
    (loop [row 0
           col 0
           count 0]
      (if (>= row rows)
        count
        (if (>= col cols)
          (recur (inc row) 0 count)
          (let [
                horizontal (if (<= (+ col word-len) cols) (subs (nth grid row) col (+ col word-len)) "")
                vertical (if (<= (+ row word-len) rows) (apply str (map #(nth % col) (subvec grid row (+ row word-len)))) "")
                diag-down-right (if (and (<= (+ row word-len) rows) (<= (+ col word-len) cols))
                                  (apply str (map #(nth (nth grid (+ row %)) (+ col %)) (range word-len)))
                                  "")
                diag-down-left (if (and (<= (+ row word-len) rows) (>= (- col word-len -1) 0))
                                 (apply str (map #(nth (nth grid (+ row %)) (- col %)) (range word-len)))
                                 "")
                
                count (+ count
                         (if (= horizontal word) 1 0)
                         (if (= (str/reverse horizontal) word) 1 0)
                         (if (= vertical word) 1 0)
                         (if (= (str/reverse vertical) word) 1 0)
                         (if (= diag-down-right word) 1 0)
                         (if (= (str/reverse diag-down-right) word) 1 0)
                         (if (= diag-down-left word) 1 0)
                         (if (= (str/reverse diag-down-left) word) 1 0))]
            (recur row (inc col) count)))))))

(defn part-1
  "Run with bb -x aoc24.day04/part-1"
  [_]
  (prn (count-occurrences input)))

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
