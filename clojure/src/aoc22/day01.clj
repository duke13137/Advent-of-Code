(ns aoc22.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(require '[malli.core :as m])
(require '[malli.instrument :as mi])

(def input (->> (slurp (io/resource "aoc22/day01.txt"))
                (str/split-lines)
                (map parse-long)))

(defn part-1
  "Run with bb -x aoc22.day01/part-1"
  [_]
  (->> input
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (apply max)
       prn))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (->> input
       (partition-by nil?)
       (take-nth 2)
       (map #(apply + %))
       (sort-by -)
       (take 3)
       (apply +)
       prn))

(part-1 nil)
(part-2 nil)

(defn kikka
  {:malli/schema [:-> :int :string]}
  [x] (str x))

(defn kakka
  {:malli/schema [:-> :string :string]}
  [x] (str x))

(kikka 1)
(kakka "1")

(mi/collect!)
