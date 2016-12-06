(ns day-6.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn pivot-data
  [data]
  (apply mapv vector data))

(defn parse-data
  [input]
  (->>
    input
    string/split-lines
    pivot-data))

(defn calc
  [sort-order data]
  (->>
    data
    (map (comp
      first
      first
      #(sort-by second sort-order %)
      frequencies))
    (apply str)))

(defn -main
  "Advent of Code - Day 6"
  [& args]
  (let [filename  "resources/input.txt"
        input     (slurp filename)
        data      (parse-data input)]
    (do
      (println (calc > data))
      (println (calc < data)))))
