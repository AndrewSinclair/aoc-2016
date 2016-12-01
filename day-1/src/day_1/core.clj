(ns day-1.core
  (require [clojure.string :as str])
  (:gen-class))

(defn get-next-direction
  [direction rotation]
  (cond
    (= rotation :right)
      (cond
        (= direction :east)  :south
        (= direction :south) :west
        (= direction :west)  :north
        (= direction :north) :east)
    (= rotation :left)
      (cond
        (= direction :east) :north
        (= direction :north) :west
        (= direction :west) :south
        (= direction :south) :east)))

(defn travel
  [direction [x y] n]
  (cond
    (= direction :east)  [(+ x n)     y   ]
    (= direction :west)  [(- x n)     y   ]
    (= direction :north) [   x     (+ y n)]
    (= direction :south) [   x     (- y n)]))

(defn get-commands
  "E.g. [[:right 1] [:left 200]]"
  [input]
  (->>
    (str/split (str/trim-newline input) #", ")
    (map #(vector
      (if (= (first %) \R) :right :left)
      (-> (subs % 1 (count %)) Integer.)))))

(defn move-command
  [[direction [x y]] [rotation distance]]
  (let [new-direction (get-next-direction direction rotation)]
    [new-direction (travel new-direction [x y] distance)]))

(defn not-distinct-key
  [xs]
  (loop [visited       #{}
         [head & tail] xs]
    (if (get visited head)
      head
      (recur (conj visited) tail))))

(defn distance-from
  [[x1 y1] [_ [x2 y2]]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn calc-part-1
  "Rotate and walk forward some number, repeat. How far did you displace?"
  [commands]
  (let [start-pos       [0 0]
        start-direction :north]
    (->>
      commands
      (reduce move-command [start-direction start-pos])
      (distance-from start-pos))))

(defn calc-part-2
  "Rotate and walk forward some number, repeat until you have visite the
  same place twice. How far did you displace?"
  [commands]
  (let [start-pos       [0 0]
        start-direction :north
        positions (->>
                    commands
                    (reductions move-command [start-direction start-pos])
                    (map second))]
    (->>
      (not-distinct-key positions)
      first
      (distance-from start-pos))))

(defn -main
  "AOC 2016 - Day 1 - Find Easter bunny HQ"
  [& args]
  (let [filename "resources/input.txt"
        input    (slurp filename)
        commands (get-commands input)]
    (do
      (println (calc-part-1 commands))
      (println (calc-part-2 commands)))))
