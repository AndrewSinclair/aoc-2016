(ns day-12.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn cpy
  [a reg]
  (fn [registers pc]
    (vector
      (inc pc)
      (if (number? a)
        (assoc registers reg a)
        (assoc registers reg (get registers a))))))

(defn jnz
  [a n]
  (fn [registers pc]
    (vector
      (cond
        (and (number? a) (not (zero? a)))
          (+ n pc)
        (and (not (number? a)) (not (zero? (get registers a))))
          (+ n pc)
        :else (inc pc))
      registers)))

(defn inc-fn
  [reg]
  (fn [registers pc]
    (vector
      (inc pc)
      (update-in registers [reg] inc))))

(defn dec-fn
  [reg]
  (fn [registers pc]
    (vector
      (inc pc)
      (update-in registers [reg] dec))))

(defn parse-number
  [num-str]
  (if (re-find #"-?[0-9]+" num-str) (Integer. num-str)))

(defn parse-line
  [line]
  (cond
    (string/starts-with? line "cpy")
      (let [[_ n reg] (re-matches #"cpy (-?[a-z0-9]+) (\w+)" line)
            n  (or (parse-number n) n)]
        (cpy n reg))
    (string/starts-with? line "jnz")
      (let [[_ n m] (re-matches #"jnz (-?[a-z0-9]+) (-?\d+)" line)
           n (or (parse-number n) n)
           m (Integer. m)]
        (jnz n m))
    (string/starts-with? line "inc")
      (let [[_ reg] (re-matches #"inc (\w+)" line)]
        (inc-fn reg))
    (string/starts-with? line "dec")
      (let [[_ reg] (re-matches #"dec (\w+)" line)]
        (dec-fn reg))))

(defn parse-input
  [input]
  (->>
    input
    string/split-lines
    (map parse-line)))

(def instructions (parse-input (slurp "resources/input.txt")))

(defn compute
  [instructions start-registers]
  (let [max-pc    (count instructions)
        pc        0
        registers start-registers]
    (loop [pc         pc
           registers  registers]
      (let [[pc registers] ((nth instructions pc) registers pc)]
        (if (>= pc max-pc)
          registers
          (recur pc registers))))))

(defn -main
  "Advent of Code - Day 12 - Bunny-Assembly"
  [& args]
  (do
    (println (compute instructions {"a" 0 "b" 0 "c" 0 "d" 0}))
    (println (compute instructions {"a" 0 "b" 0 "c" 1 "d" 0}))))
