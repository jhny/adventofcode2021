(ns day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def parse-element (comp keyword str))
(defn parse-elements [s]
  (mapv parse-element s))

(defn parse-row [s]
  (let [xs (str/split s #" -> ")]
    [(parse-elements (first xs)) (parse-element (second xs))]))





(defn parse-input [file-name]
  (let [[first & rest] (->> file-name
                         io/resource
                         slurp
                         (str/split-lines)
                         (remove empty?))]
    {:rules (into {} (mapv parse-row rest))
     :start (parse-elements first)}))


(def test-input (parse-input "day14/test_input.txt"))
(def input (parse-input "day14/input.txt"))


(defn get-pairs [{:keys [start]}]
  (->> start
    (map-indexed (fn [i x]
                   (when (contains? start (inc i))
                     [x (nth start (inc i)) (inc i)])))
    butlast))

(defn insert-at [{:keys [start] :as input} n element]
  (let [[xs ys] (split-at n start)]
    (assoc input :start (vec (concat xs [element] ys)))))



(defn step [input]
  (let [{:keys [start rules] :as input} input
        xs (get-pairs input)
        result (reduce
                 (fn [[input amount] x]
                   (let [pair (vec (take 2 x))
                         n (+ (last x) amount)
                         element (get rules pair)]
                     (if element
                       [(insert-at input n element) (inc amount)]
                       [input amount])))
                 [input 0]
                 xs)]
    (first result)))

(defn calc-score [input]
  (let [m (->> (reduce
                 (fn [input _]
                   (first (step input)))
                 input
                 (range 40))
            :start
            frequencies)
        xs (vals m)]
    (-
      (apply max xs)
      (apply min xs))))

(comment

  (calc-score test-input)
  (calc-score input)

  nil)

;; =============================================================================
;; Part two

(defn get-flat-pairs [{:keys [start]}]
  (->> start
    (map-indexed (fn [i x]
                   (when (contains? start (inc i))
                     [x (nth start (inc i))])))
    butlast))

(defn add-results [{:keys [rules] :as input}]
  (let [results (reduce-kv
                  (fn [acc k v]
                    (assoc acc k [[(first k) v] [v (second k)]]))
                  {}
                  rules)]
    (assoc input :results results)))

(defn add-result [input]
  (let [result (let [pairs (get-flat-pairs input)]
                 (reduce
                   (fn [acc x]
                     (assoc acc x 1))
                   {}
                   pairs))]
    (assoc input :result result)))


(defn calc-step [{:keys [result results] :as input}]
  (let [result (reduce-kv
                 (fn [acc k v]
                   (let [[x y] (get results k)]
                     (-> acc
                       (update y (fnil + 0) v)
                       (update x (fnil + 0) v))))
                 {}
                 result)]
    (assoc input :result result)))

(defn sum [{:keys [result start]}]
  (let [last (last start)
        result (reduce-kv
                 (fn [acc [x y] v]
                   (-> acc
                     (update x (fnil + 0) v)))
                 {}
                 result)]
    (update result last inc)))

(defn calc-part-two [input]
  (let [xs (->> (reduce
                  (fn [input _]
                    (calc-step input))
                  (add-result (add-results input))
                  (range 40))
             (sum)
             (vals))]
    (-
      (apply max xs)
      (apply min xs))))

(comment

  (calc-part-two test-input)

  (calc-part-two input)


  nil)


