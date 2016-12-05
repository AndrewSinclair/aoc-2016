(ns day-5.core
  (:require [digest])
  (:gen-class))

(defn pp-part2-password
  [password]
  (->>
    password
    (reduce #(assoc %1 (Integer. (str (first %2))) (second %2)) (apply vector (replicate 8 "_")))
    (apply str)))

(defn reduce-password
  [acc [pos chr]]
  (if-not (get acc pos)
    (let [acc' (assoc acc pos chr)]
      (do
        (println (pp-part2-password acc'))
        (if (= (count acc') 8)
          (reduced acc')
          acc')))
    acc))

(defn calc-part-1
  [input]
  (->>
    (range)
    (map #(digest/md5 (str input %)))
    (filter #(.startsWith % (apply str (replicate 5 "0"))))
    (take 8)
    (map #(nth % 5))
    (apply str)))

(defn calc-part-2
  [input]
  (->>
    (range)
    (map #(digest/md5 (str input %)))
    (filter #(.startsWith % (apply str (replicate 5 "0"))))
    (map #(subs % 5 7))
	(filter #(and (Character/isDigit (first %)) (< (Integer. (str (first %))) 8)))
	(map #(vector (Integer. (str (first %))) (second %)))
    (reduce reduce-password (sorted-map))
    pp-part2-password))

(defn -main
  "Advent of Code - Day 5 - Hash algorithm"
  [& args]
  (let [input  "abbhdwsy"]
    (do
      (println (calc-part-1 input))
      (println (calc-part-2 input)))))
