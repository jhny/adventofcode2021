(ns day17
  (:require [clojure.string :as str]))

(defn parse-input [s]
  (let [xs (-> s
             (str/replace "target area: " "")
             (str/replace "," "")
             (str/replace "y=" "")
             (str/replace "x=" "")
             (str/replace " " "..")
             (str/split #"\.\."))
        x (mapv parse-long (take 2 xs))
        y (mapv parse-long (drop 2 xs))]
    {:begin [(first x) (first y)]
     :end   [(second x) (second y)]}))


(def test-input (parse-input "target area: x=20..30, y=-10..-5"))
(def input (parse-input "target area: x=277..318, y=-92..-53"))

(defn is-within? [{:keys [begin end pos] :as input}]
  (let [[x y] pos
        [x1 y1] begin
        [x2 y2] end]
    (and (<= x x2) (>= x x1)
      (<= y y2) (>= y y1))))

(defn too-far? [{:keys [begin end pos]}]
  (let [[_ y] pos
        [_ y1] begin
        [_ y2] end]
    (and
      (> y2 y) (> y1 y))))

(defn do-step-velocity [[x y]]
  [(if (pos? x) (dec x) x)
   (dec y)])

(defn do-step [{:keys [vel pos] :as m}]
  (let [[x1 y1] vel
        [x y] pos
        new-pos [(+ x x1)
                 (+ y y1)]]
    (-> m
      (update :path conj new-pos)
      (assoc :pos new-pos)
      (update :vel do-step-velocity))))

(defn highest-step [{:keys [path] :as m}]
  (let [i (apply max (map second path))]
    (assoc m :highest i)))

(def xs (reduce
          concat
          (mapv
            (fn [y]
              (mapv
                (fn [x]
                  [x y])
                (range 400)))
            (range -100 400))))

(defn calc [input vel]
  (let [input (assoc input
                :path [[0 0]]
                :vel vel
                :pos [0 0])]
    (loop [m input]
      (cond
        (is-within? m)
        (highest-step m)
        (too-far? m)
        (assoc m :highest nil)

        :else
        (recur (do-step m))))))

(comment


  (->> xs
    (map
      (fn [vel]
        (calc input vel)))
    (filter :highest)
    (map :vel)
    (count))


  nil)


