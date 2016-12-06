(ns day-6.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn pivot-data
  [data]
  (->>
    (concat
      (map #(nth % 0) data)
      (map #(nth % 1) data)
      (map #(nth % 2) data)
      (map #(nth % 3) data)
      (map #(nth % 4) data)
      (map #(nth % 5) data)
      (map #(nth % 6) data)
      (map #(nth % 7) data))
    (partition 624)))

(defn parse-data
  [input]
  (->>
    input
    string/split-lines
    pivot-data))

(defn calc-part-1
  [data]
  (->>
    data
    (map frequencies)
    (map #(sort-by second > %))
    (map first)))

(defn calc-part-2
  [data]
  (->>
    data
    (map frequencies)
    (map #(sort-by second < %))
    (map first)))

(defn -main
  "Advent of Code - Day 6"
  [& args]
  (let [filename  "resources/input.txt"
        input     (slurp filename)
        data      (parse-data input)]
    (do
      (println (calc-part-1 data))
      (println (calc-part-2 data)))))
