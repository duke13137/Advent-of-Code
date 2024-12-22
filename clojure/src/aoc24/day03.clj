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

(defn- find-next-instruction [input]
  (let [do-index (str/index-of input "do()")
        dont-index (str/index-of input "don't()")]
    (cond
      (and (nil? do-index) (nil? dont-index)) nil
      (nil? do-index) [:dont dont-index]
      (nil? dont-index) [:do do-index]
      (< do-index dont-index) [:do do-index]
      :else [:dont dont-index])))

(defn- process-chunk [input enabled? sum]
  (if-let [[instruction-type index] (find-next-instruction input)]
    (let [chunk (subs input 0 index)
          remaining-input (subs input (+ index (if (= instruction-type :do) 4 6)))]
      [(+ sum (execute-mul chunk enabled?))
       (= instruction-type :do)
       remaining-input])
    [(+ sum (execute-mul input enabled?)) enabled? nil]))

(defn part-2
  "Run with bb -x aoc24.day03/part-2"
  [_]
  (loop [input input
         sum 0
         enabled? true]
    (let [[new-sum new-enabled? remaining-input] (process-chunk input enabled? sum)]
      (if (nil? remaining-input)
        (prn new-sum)
        (recur remaining-input new-sum new-enabled?)))))

(comment

  (part-1 nil)
  (part-2 nil)

  'comment)  
  