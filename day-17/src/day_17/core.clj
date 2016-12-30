(ns day-17.core
  (:require [digest])
  (:gen-class))

(def input "qtetzkpl")
(def empty-queue (clojure.lang.PersistentQueue/EMPTY))

(defn next-moves
  [md5-hash]
    (->>
      md5-hash
      (take 4)
      (map #(.contains "bcdef" (str %)))))

(defn in-maze?
  [x y]
  (and
    (not (neg? x))
    (not (neg? y))
    (<= x 3)
    (<= y 3)))

(defn update-queue-dir
  [path direction symbol x y queue]
  (if (and (in-maze? x y) direction)
    (conj queue {:path  (str path symbol)
                 :x     x
                 :y     y})
    queue))

(defn update-queue
  [queue path x y [up down left right]]
    (if (= x y 3)
      queue
      (->>
        queue
        (update-queue-dir path up    "U"      x (dec y))
        (update-queue-dir path down  "D"      x (inc y))
        (update-queue-dir path left  "L" (dec x)     y)
        (update-queue-dir path right "R" (inc x)     y))))

(defn bfs-iterator
  [[{path :path x :x y :y} & rest-queue]]
    (->>
      path
      (str input)
      digest/md5
      next-moves
      (update-queue (apply conj empty-queue rest-queue) path x y)))

(defn iterator-end-condition
  [[{_ :path x :x y :y} & _]]
  (= x y 3))

(defn calc-part-1
  "Find shortest path. I use a breadth-first-search (bfs) so it always
   expands the search state tree by the shortest nodes first, hence when
   it finds a path that reaches the vault, all paths shorter than it 
   have been checked already and confirmed to not reach the vault."
   []
  (->>
    {:path "" :x 0 :y 0}
    (conj empty-queue)
    (iterate bfs-iterator)
    (filter iterator-end-condition)
    first
    first
    :path))

(defn calc-part-2
  "Find longest path (and return the length). I use the same BFS as in part 1,
  but I don't stop at the first path this time. Instead I generate all paths
  (for which BFS is neither better nor worse than other strategies), then filter
  on which finish at the vault, then sort the path lengths.
  Note, generating all paths in the BFS means iterating until the queue is empty,
  so long as no illegal paths are ever entered into the queue."
  []
  (->>
    {:path "" :x 0 :y 0}
    (conj empty-queue)
    (iterate bfs-iterator)
    (take-while (comp not empty?))
    (filter (fn [[{x :x y :y}]] (= x y 3)))
    (map #(-> % first :path count))
    (sort >)
    first))

(defn -main
  "Advent of Code - Day 17 - Vault Maze with MD5"
  [& args]
  (do
    (println (calc-part-1))
    (println (calc-part-2))))
