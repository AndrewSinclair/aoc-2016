(ns day-12.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn parse-line
  [line]
  (cond
    (string/starts-with? line "cpy")
      (let [[_ n reg] (re-matches #"cpy ([\-a-z0-9]+) (\w+)" line)] '(cpy (Integer. n) reg))
    (string/starts-with? line "jnz")
      (let [[_ n reg] (re-matches #"jnz ([\-a-z0-9]+) (\w+)" line)] '(jnz (Integer. n) reg))
    (string/starts-with? line "inc")
      (let [[_ reg] (re-matches #"inc (\w+)" line)] '(inc reg))
    (string/starts-with? line "dec")
      (let [[_ reg] (re-matches #"dec (\w+)" line)] '(dec reg))))


(defn parse-input
  [input]
  (->>
    input
    string/split-lines
    (map parse-line)))

(def instructions (parse-input (slurp "resources/input.txt")))

(defn calc-part-1
  [instructions]
  nil)

(defn calc-part-2
  [instructions]
  nil)

(defn -main
  "Advent of Code - Day 12 - Bunny-Assembly"
  [& args]
  (do
    (println (calc-part-1 instructions))
    (println (calc-part-2 instructions))))
