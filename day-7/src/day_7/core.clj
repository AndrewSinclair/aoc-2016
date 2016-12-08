(ns day-7.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn abba-reducer
  [_ [a b b' a']]
  (if (and
        (= a a')
        (= b b')
        (not= a b))
    (reduced true)))

(defn is-abba?
  [text]
  (->>
    text
    (partition 4 1)
    (reduce abba-reducer false)))

(defn aba-reducer
  [hypernets _ [a b a']]
  (if (and
        (= a a')
        (not= a b)
        (some #(if % (.contains % (str b a b))) hypernets))
    (reduced true)))

(defn is-aba-bab?
  [supernet hypernets]
  (->>
    supernet
    (partition 3 1)
    (reduce (partial aba-reducer hypernets) false)))

(defn parse-input
  [input]
  (->>
    input
    string/split-lines
    (map #(string/split % #"[\[\]]"))
    (map #(partition-all 2 %))
    (map #(vector (map first %) (map second %)))))

(defn calc-part-1
  [data]
  (->>
    data
    (filter
      (fn [[supernets hypernets]]
        (and
          (some is-abba? supernets)
          (every? (complement is-abba?) hypernets))))
    count))

(defn calc-part-2
  [data]
  (->>
    data
    (filter
      (fn [[supernets hypernets]]
         (some #(is-aba-bab? % hypernets) supernets)))
    count))

(defn -main
  "Advent of Code - Day 7 - IPv7 (no it's not a real thing)"
  [& args]
  (let [filename "resources/input.txt"
        input    (slurp filename)
        data     (parse-input input)]
    (do
      (println (calc-part-1 data))
      (println (calc-part-2 data)))))
