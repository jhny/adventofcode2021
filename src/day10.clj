(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-row [s]
  (mapv str s))
(defn parse-input [file-name]
  (mapv parse-row (-> file-name
                    io/resource
                    slurp
                    (str/split-lines))))

(def test-input (parse-input "day10/test_input.txt"))
(def input (parse-input "day10/input.txt"))
(def test-case (parse-row "{([(<{}[<>[]}>{[]{[(<()>"))
(def test-case2 (parse-row "[({(<(())[]>[[{[]{<()<>>"))

(def start? #{"{" "(" "<" "["})
(def end? #{"}" ")" ">" "]"})

(def opposite? {"}" "{" ")" "(" ">" "<" "]" "["})
(def opposite-from-start {"{" "}" "(" ")" "<" ">" "[" "]"})

(def score-table
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(def score-table2
  {")" 1
   "]" 2
   "}" 3
   ">" 4})

(defn reduce-syntax [xs]
  (reduce
    (fn [acc x]
      (cond
        (start? x)
        (conj acc x)

        (and (end? x)
          (= (first acc) (opposite? x)))
        (rest acc)

        :else
        (reduced x)))
    (list)
    xs))

(defn is-corrupted [xs]
  (let [result (reduce-syntax xs)]
    (when (string? result)
      result)))

(defn get-incomplete-list [xs]
  (let [result (reduce-syntax xs)]
    (when (list? result)
      result)))


(defn calculate-score [input]
  (->> input
    (map is-corrupted)
    (remove nil?)
    (map score-table)
    (apply +)))

(comment

  (calculate-score test-input)
  (calculate-score input)

  nil)

(defn calc-score-for-incomplete [xs]
  (when (seq xs)
    (reduce
      (fn [acc x]
        (+ (* acc 5) x))
      0
      xs)))

(defn calc-single-entry-score [xs]
  (->> xs
    (get-incomplete-list)
    (map opposite-from-start)
    (map score-table2)
    (calc-score-for-incomplete)))

(defn calculate-score2 [input]
  (let [xs (->> input
             (map calc-single-entry-score)
             (remove nil?)
             (sort))
        i (/ (dec (count xs)) 2)]
    (nth xs i)))

(comment

  (calculate-score2 test-input)

  (calculate-score2 input)

  nil)







