(ns aoc24.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (slurp (io/resource "aoc24/day04.txt")))

(defn parse-grid [input]
  (str/split-lines input))

(defn- extract-string [grid row col word-len dir-row dir-col]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [i 0
           s ""]
      (if (>= i word-len)
        s
        (let [new-row (+ row (* i dir-row))
              new-col (+ col (* i dir-col))]
          (if (and (>= new-row 0) (< new-row rows) (>= new-col 0) (< new-col cols))
            (recur (inc i) (str s (get-in grid [new-row new-col])))
            ""))))))

(defn count-occurrences [grid word]
  (let [rows (count grid)
        cols (count (first grid))
        word-len (count word)
        directions [[0 1] [1 0] [1 1] [1 -1]]
        mutable-count (atom 0)]
    (doseq [row (range rows)]
      (doseq [col (range cols)]
        (doseq [[dir-row dir-col] directions]
          (let [forward (extract-string grid row col word-len dir-row dir-col)
                backward (str/reverse forward)]
            (when (= forward word)
              (swap! mutable-count inc))
            (when (= backward word)
              (swap! mutable-count inc))))))
    @mutable-count))

(defn- check-diagonal-pair [grid row col up-dir-row up-dir-col down-dir-row down-dir-col word reversed-word]
  (let [up-diag (extract-string grid (+ row up-dir-row) (+ col up-dir-col) (count word) up-dir-row up-dir-col)
        down-diag (extract-string grid (+ row down-dir-row) (+ col down-dir-col) (count word) down-dir-row down-dir-col)]
    (and (or (= up-diag word) (= up-diag reversed-word))
         (or (= down-diag word) (= down-diag reversed-word)))))

(defn- is-xmas-shape [grid row col]
  (let [word "MAS"
        reversed-word (str/reverse word)]
    (or (check-diagonal-pair grid row col -1 -1 1 1 word reversed-word)
        (check-diagonal-pair grid row col -1 1 1 -1 word reversed-word))))

(defn count-xmas-shapes [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (reduce
     (fn [count row]
       (+ count (reduce
                 (fn [col-count col]
                   (+ col-count (if (is-xmas-shape grid row col) 1 0)))
                 0 (range 1 (- cols 1)))))
     0 (range 1 (- rows 1)))))

(defn part-1 [_]
  (prn (count-occurrences (parse-grid input) "XMAS")))

(defn part-2 [_]
  (prn (count-xmas-shapes (parse-grid input))))
