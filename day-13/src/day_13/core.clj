(ns day-13.core
  (:gen-class))

(def max-width 50)

;; true means open, false means walled
(def maze
  (->>
    (for [y (range 0 max-width)
          x (range 0 max-width)]
      (+
        (* x x)
        (* x 3)
        (* x y 2)
        y
        (* y y)
        1350))
      (map #(Integer/toString % 2))
      (map #(even? (count (filter (fn [m] (= m \1)) %))))))

(defn get-x-y-is-valid?
  [maze x y]
  (if (or (neg? x) (neg? y)) false
    (nth maze (+ y (* x max-width)))))

(defn move-valid?
  [maze max-moves prev-moves [curr-move [x y]]]
  (and (<= curr-move max-moves)
       (get-x-y-is-valid? maze x y)
       (not (contains? prev-moves [x y]))))

(defn get-next-moves
  "returns all the next x and y.
   Does not do any filtering for invalid moves.
   Also prepends the `curr-move' value."
  [curr-move x y]
  (->>
    [[(inc x) y]
     [(dec x) y]
     [x      (inc y)]
     [x      (dec y)]]
    (map #(vector (inc curr-move) %))))


(defn calc-maze-max-moves
  [maze max-moves]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [0 [1 1]])
        distinct-moves #{[1 1]}]
    (let [[curr-move [x y]] (first queue)
         next-moves         (get-next-moves curr-move x y)
         valid-next-moves   (filter #(move-valid? maze max-moves distinct-moves %) next-moves)
         queue              (reduce conj (pop queue) valid-next-moves)
         count-distinct     (count distinct-moves)
         distinct-moves     (conj distinct-moves [x y])]
      (if (empty? queue)
        count-distinct
        (recur queue distinct-moves)))))


(defn -main
  "Advent of Code - Day 13 - Maze"
  [& args]
  (do
    (println "print the maze and count the moves. It's not hard.")
    (println (calc-maze-max-moves maze 50))))
