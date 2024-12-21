(ns aoc24.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (->> (slurp (io/resource "aoc24/day05.txt"))
                (str/split-lines)))

(defn parse-rule [rule-str]
  (->> (str/split rule-str #"\|")
       (map parse-long)))

(defn parse-rules [rules-str]
  (->> (str/split-lines rules-str)
       (map parse-rule)
       (set)))

(defn applicable-rules [update rules]
  (filter (fn [[x y]] (and (some #{x} update) (some #{y} update))) rules))

(defn validate-update [update rules]
  (let [app-rules (applicable-rules update rules)]
    (every? (fn [[x y]] (< (.indexOf update x) (.indexOf update y))) app-rules)))

(defn middle-pages [update]
  (let [len (count update)
        mid (quot len 2)]
    (if (odd? len)
      [(nth update mid)]
      [(nth update (dec mid)) (nth update mid)])))

(defn sum-middle-pages [updates rules]
  (->> updates
       (filter #(validate-update % rules))
       (mapcat middle-pages)
       (reduce + 0)))

(defn solve [rules-str updates-str]
  (let [rules (parse-rules rules-str)
        updates (->> (str/split-lines updates-str)
                     (map #(->> (str/split % #",") (map parse-long))))]
    (sum-middle-pages updates rules)))

(def rules-str "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13")
(def updates-str "75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47")

(defn part-1
  "Run with bb -x aoc24.day05/part-1"
  [_]
  (prn (solve rules-str updates-str)))
