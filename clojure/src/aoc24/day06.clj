(ns aoc24.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-grid []
  (with-open [rdr (io/reader (io/resource "aoc24/day06.txt"))]
    (vec (map vec (line-seq rdr)))))

(defn find-guard [grid]
  (first (for [r (range (count grid))
               c (range (count (first grid)))
               :let [char (get-in grid [r c])]
               :when (some #{char} [\^ \> \v \<])]
           [(vector r c) char])))

;; -- AI! refactor this to be idiomatic Clojure
(defn move-guard [grid pos direction]
  (let [height (count grid)
        width (count (first grid))
        [r c] pos
        [next-r next-c] (case direction
                          \^ [(dec r) c]
                          \> [r (inc c)]
                          \v [(inc r) c]
                          \< [r (dec c)])]
    (if (and (>= next-r 0) (< next-r height)
             (>= next-c 0) (< next-c width)
             (= \# (get-in grid [next-r next-c])))
      [pos (case direction
             \^ \>
             \> \v
             \v \<
             \< \^)]
      (if (and (>= next-r 0) (< next-r height)
               (>= next-c 0) (< next-c width))
        [(vector next-r next-c) direction]
        [nil nil]))))

(defn solve []
  (let [grid (read-grid)
        [guard-pos guard-dir] (find-guard grid)]
    (loop [pos guard-pos
           direction guard-dir
           visited #{guard-pos}]
      (let [[next-pos next-dir] (move-guard grid pos direction)]
        (if (nil? next-pos)
          (count visited)
          (recur next-pos next-dir (conj visited next-pos)))))))

(defn part-1
  "Run with bb -x aoc24.day06/part-1"
  [_]
  (println (solve)))
