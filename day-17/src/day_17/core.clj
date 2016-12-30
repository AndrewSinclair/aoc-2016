(ns day-17.core
  (:require [digest])
  (:gen-class))

(def input "qtetzkpl")
(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn next-moves
  [md5-hash]
    (->>
      md5-hash
      (take 4)
      (map #(.contains "bcdef" (str %)))))

(defn update-queue
  [queue path up down left right x y]
  (let [queue  (if (and up    (>  y 0)) (conj queue [(str path "U")  x       (dec y)]) queue)
        queue  (if (and down  (<= y 3)) (conj queue [(str path "D")  x       (inc y)]) queue)
        queue  (if (and left  (>  x 0)) (conj queue [(str path "L") (dec x) y      ]) queue)
        queue  (if (and right (<= x 3)) (conj queue [(str path "R") (inc x) y      ]) queue)]
    queue))

(defn calc-part-1
  [input]
  (loop [queue (conj empty-queue ["" 0 0])]
    (let [[path x y]            (first queue)
          md5-hash              (digest/md5 (str input path))
          [up down left right]  (next-moves md5-hash)
          queue                 (update-queue (pop queue) path up down left right x y)]
      (cond
        (empty? queue)        "There is no exit"
        (and (= x 3) (= y 3)) path
        :else                 (recur queue)))))

(defn calc-part-2
  [input]
  nil)

(defn -main
  "Advent of Code - Day 17 - Vault Maze with MD5"
  [& args]
  (do
    (println (calc-part-1 input))
    (println (calc-part-2 input))))
