(ns day-19.core
  (:gen-class))

(defn lassoc [list n value]
  (let [[_ & tail] (drop n list)]
    (concat (take n list)
            (cons value tail))))

(defn steal-presents
  [iterations current elves]
  (if (zero? iterations)
    [current elves]
    (let [live-elves      (keep-indexed #(when (= %2 :live) [%1 %2]) elves)
          num-live        (count live-elves)
          next-dead-index (-> current (+ (quot num-live 2)) (mod num-live))
          next-dead-id    (first (nth live-elves next-dead-index))
          elves           (lassoc elves next-dead-id :dead)
          current         (or (->> elves (keep-indexed #(when (and (> %1 current) (= %2 :live)) %1)) first)
                              (->> elves (keep-indexed #(when  (= %2 :dead) %1)) first))] 
      (recur (dec iterations) current elves))))

(defn calc-part-1
  [input]
  "use the formula: if n = 2^m + l, then f(n) = 2l + 1")

(defn calc-part-2
  [input]
  (->>
    (repeat input :live)
    (steal-presents (dec input) 0)
    first
    inc))

(defn -main
  "Advent of Code - Day 19 - Josephus Elf"
  [& args]
  (let [input 3004953]
    (do
      (println (calc-part-1 input))
      (println (calc-part-2 5)))))
