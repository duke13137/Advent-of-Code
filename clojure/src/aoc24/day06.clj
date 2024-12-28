(ns aoc24.day06
  (:require [clojure.string :as str]))

(defn read-grid [input]
  (vec (map vec (str/split-lines input))))

(defn find-guard [grid]
  (first (for [r (range (count grid))
               c (range (count (first grid)))
               :let [char (get-in grid [r c])]
               :when (some #{char} [\^ \> \v \<])]
           [(vector r c) char])))

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

(defn part-1 [input]
  (let [grid (read-grid input)
        [guard-pos guard-dir] (find-guard grid)]
    (loop [pos guard-pos
           direction guard-dir
           visited #{guard-pos}]
      (let [[next-pos next-dir] (move-guard grid pos direction)]
        (if (nil? next-pos)
          (count visited)
          (recur next-pos next-dir (conj visited next-pos)))))))

(def input (slurp "resources/aoc24/day06.txt"))

(def example "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(part-1 example)
;; (part-2 example)

(part-1 input)
;; (part-2 input)
