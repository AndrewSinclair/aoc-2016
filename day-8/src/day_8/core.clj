(ns day-8.core
  (:require [clojure.string :as string])
  (:gen-class))

(def rows 6)
(def cols 50)
(def start-screen (vec (take (* rows cols) (repeat :off))))

(defn pp-screen
  [screen]
  (map #(println (map (fn [n] (if (= n :on) \# \.)) %)) (partition cols screen)))

(defn get-column
  [n matrix]
  (->>
    matrix
    (partition cols)
    (map #(nth % n))))

(defn set-column
  [n matrix col]
  (reduce #(assoc %1 (+ n (* %2 cols)) (get col %2)) matrix (range rows)))

(defn set-lights
  [state n m matrix]
  (->>
    (for [i (range n)
          j (range m)]
      (+ i (* j cols)))
    (reduce #(assoc %1 %2 state) matrix)))

(defn rotate
  [n xs]
  (->>
    xs
    cycle
    (drop (- (count xs) n))
    (take (count xs))))

(defn rotate-row
  [matrix idx n]
  (let [rows  (->>
                matrix
                (partition cols)
                vec)]
    (->>
      (update rows idx (partial rotate n))
      (apply concat)
      vec)))

(defn rotate-col
  [matrix idx n]
  (set-column idx matrix
    (->>
      matrix
      (get-column idx)
      (rotate n)
      vec)))

(defn parse-rotation
  [text]
  (let [[_ i n] (re-matches #".*?(\d+) by (\d+)" text)
        i (Integer. i)
        n (Integer. n)]
    (if (.startsWith text "rotate row")
      (fn [matrix] (rotate-row matrix i n))
      (fn [matrix] (rotate-col matrix i n)))))

(defn parse-rectangle
  [text]
  (let [[_ n m] (re-matches #".*?(\d+)x(\d+)" text)
       n (Integer. n)
       m (Integer. m)]
    (fn [matrix] (set-lights :on n m matrix))))

(defn make-fn
  [text]
  (cond
    (.startsWith text "rect") (parse-rectangle text)
    (.startsWith text "rotate") (parse-rotation text)
    :else nil))

(defn parse-input
  [input]
  (->>
    input
    string/split-lines
    (map make-fn)))

(def data (parse-input (slurp "resources/input.txt")))

(defn calc-part-1
  [data screen]
  (->>
    screen
    ((apply comp (reverse data)))
    (filter #(= % :on))
    count))

(defn calc-part-2
  [data screen]
  (->>
    screen
    ((apply comp (reverse data)))
    pp-screen))

(defn -main
  "Advent of Code - Day 8 - Tiny Screen"
  [& args]
  (do
    (println (calc-part-1 data start-screen))
    (println (calc-part-2 data start-screen))))
