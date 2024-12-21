(ns aoc24.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [file]
  (str/trim (slurp (io/resource file))))

(defn execute-mul [input enabled?]
  (let [matches (re-seq #"mul\((\d+),(\d+)\)" input)]
    (reduce (fn [sum [_ a b]]
              (if enabled?
                (+ sum (* (parse-long a) (parse-long b)))
                sum))
            0
            matches)))

(def input (parse-input "aoc24/day03.txt"))

(defn part-1
  "Run with bb -x aoc24.day03/part-1"
  [_]
  (prn (execute-mul input true)))

(defn- process-segment [input enabled? sum]
  (let [do-index (str/index-of input "do()")
        dont-index (str/index-of input "don't()")
        [next-index new-enabled?] (condp = [do-index dont-index]
                                    [nil nil] [nil enabled?]
                                    [nil 0] [dont-index false]
                                    [0 nil] [do-index true]
                                    (if (< do-index dont-index)
                                      [do-index true]
                                      [dont-index false]))]
    (if (nil? next-index)
      [(+ sum (execute-mul input enabled?)) enabled?]
      [(+ sum (execute-mul (subs input 0 next-index) enabled?))
       new-enabled?
       (subs input (+ next-index (if new-enabled? 4 6)))])))

(defn part-2
  "Run with bb -x aoc24.day03/part-2"
  [_]
  (loop [input input
         sum 0
         enabled? true]
    (let [[new-sum new-enabled? remaining-input] (process-segment input enabled? sum)]
      (if (nil? remaining-input)
        (prn new-sum)
        (recur remaining-input new-sum new-enabled?)))))
