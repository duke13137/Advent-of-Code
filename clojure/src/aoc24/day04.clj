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

(defn is-xmas-shape [grid row col]
  (let [rows (count grid)
        cols (count (first grid))]
    (if (or (< row 1) (>= row (dec rows)) (< col 1) (>= col (dec cols)))
      false
      (let [center (get-in grid [row col])
            word "MAS"
            reversed-word (str/reverse word)]
        (if (not= center \A)
          false
          (let [
                top (get-in grid [(dec row) col])
                bottom (get-in grid [(inc row) col])
                left (get-in grid [row (dec col)])
                right (get-in grid [row (inc col)])
                top-left (get-in grid [(dec row) (dec col)])
                top-right (get-in grid [(dec row) (inc col)])
                bottom-left (get-in grid [(inc row) (dec col)])
                bottom-right (get-in grid [(inc row) (inc col)])
                ]
            (cond
              (and (= top \M) (= bottom \M) (= left \S) (= right \S)) true
              (and (= top \S) (= bottom \S) (= left \M) (= right \M)) true
              (and (= top \M) (= bottom \M) (= left \S) (= right \S)) true
              (and (= top \S) (= bottom \S) (= left \M) (= right \M)) true
              (and (= top-left \M) (= bottom-right \M) (= top-right \S) (= bottom-left \S)) true
              (and (= top-left \S) (= bottom-right \S) (= top-right \M) (= bottom-left \M)) true
              (and (= top-left \M) (= bottom-right \M) (= top-right \S) (= bottom-left \S)) true
              (and (= top-left \S) (= bottom-right \S) (= top-right \M) (= bottom-left \M)) true
              :else false
              )))))))

(defn count-xmas-shapes [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [row 0
           col 0
           count 0]
      (if (>= row rows)
        count
        (if (>= col cols)
          (recur (inc row) 0 count)
          (recur row (inc col) (if (is-xmas-shape grid row col) (inc count) count)))))))

(defn part-1
  "Run with bb -x aoc24.day04/part-1"
  [_]
  (prn (count-occurrences input)))

(defn part-2
  "Run with bb -x aoc24.day04/part-2"
  [_]
  (prn (count-xmas-shapes input)))
