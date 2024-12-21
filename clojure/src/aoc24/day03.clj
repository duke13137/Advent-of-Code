(ns aoc24.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [file]
  (str/trim (slurp (io/resource file))))

(defn execute-mul [line enabled?]
  (let [matches (re-seq #"mul\((\d+),(\d+)\)" line)]
    (reduce (fn [sum [_ a b]]
              (if enabled?
                (+ sum (* (parse-long a) (parse-long b)))
                sum))
            0
            matches)))

(defn part-1 [input]
  (execute-mul input true))

(defn part-2 [input]
  (loop [lines (str/split-lines input)
         sum 0
         enabled? true]
    (if (empty? lines)
      sum
      (let [line (first lines)
            new-enabled? (cond
                           (str/includes? line "do()") true
                           (str/includes? line "don't()") false
                           :else enabled?)
            mul-result (execute-mul line new-enabled?)]
        (recur (rest lines) (+ sum mul-result) new-enabled?)))))

(defn -main [& _]
  (let [input (parse-input "aoc24/day03.txt")]
    (println "Part 1:" (part-1 input))
    (println "Part 2:" (part-2 input))))
