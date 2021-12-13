(ns day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-row [s]
  (cond
    (empty? s)
    nil

    (str/starts-with? s "fold along ")
    (let [xs (-> s
               (str/replace-first "fold along " "")
               (str/split #"="))]
      [(keyword (first xs)) (parse-long (second xs))])

    :else
    (mapv parse-long (str/split s #","))))

(defn place-point [xs [x y]]
  (update xs y assoc x 1))

(defn create-grid [xs]
  (let [ys (take-while #(not (keyword? (first %))) xs)
        maxx (apply max (map first ys))
        maxy (apply max (map second ys))
        grid (mapv
               (fn [_]
                 (mapv (fn [_] 0) (range (inc maxx))))
               (range (inc maxy)))]
    {:grid  (reduce
              place-point
              grid
              ys)
     :folds (vec (drop-while #(not (keyword? (first %))) xs))}))

(defn parse-input [file-name]
  (->> file-name
    io/resource
    slurp
    (str/split-lines)
    (map parse-row)
    (remove nil?)))

(def test-input (parse-input "day13/test_input.txt"))
(def input (parse-input "day13/input.txt"))
(def repeately-zero (repeatedly (fn [] 0)))

(defn fixy [y1 y2]
  (let [x1 (count y1)
        x2 (count y2)]
    (if (> x2 x1)
      (concat y1
        (mapv (fn [_] (vec (take (count (first y1)) repeately-zero)))
          (range (- x2 x1))))
      y1)))

(defn fold-y [grid [_ y]]
  (let [y1 (take y grid)
        y2 (drop (inc y) grid)
        y2 (fixy y2 y1)
        y2 (reverse y2)]

    (mapv (fn [xs1 xs2]
            (mapv max xs2 xs1))
      y2 (fixy y1 y2))))

(defn fixx [x1 x2]
  (let [y1 (count (first x1))
        y2 (count (first x2))]
    (if (> y2 y1)
      (mapv (fn [xs] (vec (concat xs (take (- y2 y1) repeately-zero)))) x1)
      x1)))

(defn fold-x [grid [_ x]]
  (let [x1 (mapv (fn [xs] (vec (take x xs))) grid)
        x2 (mapv (fn [xs] (vec (drop (inc x) xs))) grid)

        x1 (mapv (fn [xs] (vec xs)) x1)
        x2 (fixx x2 x1)
        x2 (mapv (fn [xs] (vec (reverse xs))) x2)]

    (mapv (fn [xs1 xs2]
            (mapv max xs2 xs1))
      x2 (fixx x1 x2))))

(defn count-points [grid]
  (reduce
    (fn [acc xs]
      (reduce
        +
        acc
        xs))
    0
    grid))

(let [{:keys [grid]} (create-grid test-input)
      grid (fold-y grid [:y 7])]
  (fold-x grid [:x 5]))

(defn fold [grid [k _ :as fold]]
  (if (= k :y)
    (fold-y grid fold)
    (fold-x grid fold)))

(defn flip [xxs]
  (mapv (fn [xs] (mapv (fn [x] (if (= x 0) 1 0)) xs)) xxs))


(comment

  (let [{:keys [grid folds]} (create-grid input)]
    (flip (reduce fold grid folds)))


  nil)