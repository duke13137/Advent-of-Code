(ns aoc22.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(require '[com.fulcrologic.guardrails.core :refer [>defn >def | ? =>]])
(require '[playback.core])

(>def ::thing (s/or :i int? :s string?))
(>defn f [i] [::thing => int?]
  (if (string? i)
    0
    (inc i)))

(f 3)

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
