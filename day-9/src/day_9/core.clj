(ns day-9.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn parse-input
  [input]
  input)

(defn compress-length
  [acc data]
  (let [[matches i j] (re-find #"\((\d+)x(\d+)\)" data)]
    (if matches
      (let [i    (Integer. i)
            j    (Integer. j)
            data (apply str (drop (+ (count matches) i) data))
            acc  (+ (* i j) acc)]
        (recur acc data))
      acc)))

(defn solve
  [input]
  (if (or (string/blank? input) (not (.contains input "("))) ;;)
    (count input)
    (let [lparen-idx   (string/index-of input "(") ;;)(
          rparen-idx   (string/index-of input ")")
          [_ i j rest] (re-matches #".*?\((\d+)x(\d+)\)(.*)" input)
	      i            (Integer. i)
	      j            (Integer. j)]
      (+
        lparen-idx
        (* j (solve (apply str (take i rest))))
        (solve (apply str (drop i rest)))))))

(defn calc-part-1
  [data]
  (compress-length 0 data))

(defn calc-part-2
  [data]
  (solve data))

(def data (parse-input (slurp "resources/input.txt")))

(defn -main
  "Advent of Code - Day 9 - Decompressing Messages"
  [& args]
  (do
    (println (calc-part-1 data))
    (println (calc-part-2 data))))
	