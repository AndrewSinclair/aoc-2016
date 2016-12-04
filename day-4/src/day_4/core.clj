(ns day-4.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn order-alphabets
  [encryption]
  (->>
    encryption
    (filter #(Character/isLetter %))
    sort
    frequencies
    (sort-by second >)
    (map first)))

(defn destruct-line
  [line]
  (let [[_ encryption sector checksum]
          (re-matches #"(.*)-(\d+)\[(\w+)\]" line)]
    {:encryption encryption
     :ordered    (order-alphabets encryption)
     :sector     (Integer. sector)
     :checksum   checksum}))

(defn parse-input
  [input]
  (->>
    input
    str/split-lines
    (map destruct-line)))

(defn confirm-checksum
  [data-line]
  (let [encryption (take 5 (:ordered data-line))
        checksum   (:checksum data-line)]
    (->>
      (map vector encryption checksum)
      (every? (partial apply =)))))

(defn rotate
  [number alphabet]
  (->
    alphabet
    int (+ number) (- 97) (mod 26) (+ 97) char))

(defn rotate-word
  [word number]
  (->>
    word
    (map #(if (= % \-) \space (rotate number %)))
    (apply str)))

(defn calc-part-1
  [data]
  (->>
    data
    (filter confirm-checksum)
    (map :sector)
    (reduce +)))

(defn calc-part-2
  [data]
  (->>
    data
    (filter confirm-checksum)
    (map
      #(vector
        (rotate-word (:encryption %) (:sector %))
        (:sector %)))
    (filter #(.contains (first %) "northpole"))
    first
    second))

(defn -main
  "Advent of Code - Day 4 - Room Checksums"
  [& args]
  (let [filename "resources/input.txt"
        input    (slurp filename)
        data     (parse-input input)]
    (do
     (println (calc-part-1 data))
     (println (calc-part-2 data)))))
