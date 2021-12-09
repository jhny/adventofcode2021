(ns day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn parse-row [s]
  (mapv (comp parse-long str) s))
(defn parse-input [file-name]
  (mapv parse-row (-> file-name
                    io/resource
                    slurp
                    (str/split-lines))))

(def test-input (parse-input "day9/test_input.txt"))
(def input (parse-input "day9/input.txt"))

(defn get-from-coord [input [x y]]
  (try (nth (nth input y) x)
       (catch Exception _ nil)))

(defn right [input [x y]] (get-from-coord input [(inc x) y]))
(defn left [input [x y]] (get-from-coord input [(dec x) y]))
(defn up [input [x y]] (get-from-coord input [x (inc y)]))
(defn down [input [x y]] (get-from-coord input [x (dec y)]))

(defn all-adjecent [input [x y :as coord]]
  (vec (remove nil? (list
                      (right input coord)
                      (left input coord)
                      (up input coord)
                      (down input coord)))))

(defn is-lowpoint [input [x y :as coord]]
  (let [xs (all-adjecent input coord)
        x (get-from-coord input coord)]
    (every? true? (map #(< x %) xs))))


(defn get-all-lowpoints [input]
  (reduce
    (fn [acc y]
      (reduce
        (fn [acc x]
          (if (is-lowpoint input [x y])
            (conj acc [(get-from-coord input [x y]) [x y]])
            acc))
        acc
        (range (count (nth input y)))))
    []
    (range (count input))))

(comment

  (apply + (map (comp inc first) (get-all-lowpoints test-input)))

  (apply + (map (comp inc first) (get-all-lowpoints input)))

  nil)

(defn all-adjecent-coords [[x y]]
  (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]))

(defn get-flow-adjecents [input coord]
  (let [x (get-from-coord input coord)
        xs (all-adjecent-coords coord)
        xs (remove #(nil? (get-from-coord input %)) xs)
        xs (remove #(= 9 (get-from-coord input %)) xs)]
    (filter (fn [coord]
              (< x (get-from-coord input coord)))
      xs)))

(defn get-all-flow-adjecents* [input acc coord]
  (let [xs (get-flow-adjecents input coord)]
    (if (seq xs)
      (reduce
        #(get-all-flow-adjecents* input %1 %2)
        (concat acc xs)
        xs)
      acc)))

(defn get-all-flow-adjecents [input coord]
  (distinct (get-all-flow-adjecents* input [] coord)))


(defn calc-largest-basins [input]
  (->> input
    (get-all-lowpoints)
    (map #(get-all-flow-adjecents input (second %)))
    (map (fn [xs] (+ 1 (count xs))))
    (sort)
    (reverse)
    (take 3)
    (apply *)))

(comment

  (calc-largest-basins input)
  (calc-largest-basins input)

  nil)




