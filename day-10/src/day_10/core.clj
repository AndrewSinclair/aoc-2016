(ns day-10.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn parse-input
  [input]
  (->>
    input
    string/split-lines
    (map parse-input-line)
    (reduce #(cond (:value %2) (assoc-in %1 [:values] %2)
                    :else      (assoc-in %1 [:bots] %2))
    {:values {} :bots {}})))

(defn parse-input-line
  [input-line]
  (cond (.contains input "goes to")
    (let [[_ input-val bot] (re-matches #"value (\d+) goes to bot (\d+)" input-line)]
      {:value input-val :goes-to-bot bot})
    (let [[_ robot low-designation low-id high-designation high-id]  (re-matches #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)" input-line)
          low-heir [(if (= low-deisgnation "bot") :bot :output) low-id]
		  high-heir [(if (= high-deisgnation "bot") :bot :output) high-id]]
      {:bot robot :gives-low-to low-heir :gives-high-to high-heir})))

(defn resolve-bot-map
  [data]
  enter all the values first, then go through it. When a bot has two, enter them too.)

(defn calc-part-1
  [data]
  nil)

(defn calc-part-2
  [data]
  nil)

(def data (parse-input (slurp "resources/input.txt")))

(defn -main
  "Advent of Code - Day 10 - Robot Chip Compare"
  [& args]
  (do
    (println (calc-part-1 data))
    (println (calc-part-2 data))))
