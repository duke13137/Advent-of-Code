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

(defn part-2
  "Run with bb -x aoc24.day03/part-2"
  [_]
  (prn (loop [input input
              sum 0
              enabled? true]
        (let [do-index (str/index-of input "do()")
              dont-index (str/index-of input "don't()")
              next-index (cond
                           (and do-index (or (nil? dont-index) (< do-index dont-index)))
                           do-index

                           (and dont-index (or (nil? do-index) (< dont-index do-index)))
                           dont-index

                           :else
                           nil)]
          (if (nil? next-index)
            (+ sum (execute-mul input enabled?))
            (let [mul-result (execute-mul (subs input 0 next-index) enabled?)
                  new-enabled? (if (= next-index do-index) true false)
                  remaining-input (subs input (+ next-index (if (= next-index do-index) 4 6)))]
              (recur remaining-input (+ sum mul-result) new-enabled?)))))))

