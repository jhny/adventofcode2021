(ns day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.java.math :as math]))

(defn parse-list [s]
  (let [s (str/trim s)]
    (mapv (fn [x]
            (set (map (comp keyword str) x)))
      (str/split s #" "))))


(defn parse-row [s]
  (let [xs (str/split s #"\|")
        xs (mapv parse-list xs)]
    {:input  (first xs)
     :output (second xs)}))

(defn parse-input [file-name]
  (map parse-row
    (-> file-name
      io/resource
      slurp
      str/split-lines)))


(def test-input (parse-input "day8/test_input.txt"))
(def tinput (parse-row "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
(def input (parse-input "day8/input.txt"))


(def numbers
  {0 #{:a :b :c :e :f :g}
   1 #{:c :f}
   2 #{:a :c :d :e :g}
   3 #{:a :c :d :f :g}
   4 #{:b :c :d :f}

   5 #{:a :b :d :f :g}
   6 #{:a :b :d :e :f :g}
   7 #{:a :c :f}
   8 #{:a :b :c :d :e :f :g}
   9 #{:a :b :c :d :f :g}})

(def number-sizes
  (reduce-kv
    (fn [acc k v]
      (assoc acc k (count v)))
    {}
    numbers))

(defn k-to-upper [k]
  (keyword (str/upper-case (name k))))

(def nm
  (reduce-kv
    (fn [acc k v]
      (assoc acc (set (map k-to-upper v)) k))
    {}
    numbers))


(defn calculate-part-one [input]
  (let [result (->> input
                 (map
                   (fn [{:keys [output]}]
                     (map count output)))
                 flatten
                 frequencies)
        nm-sizes (vals (select-keys number-sizes [1 4 7 8]))]
    (reduce
      (fn [acc size]
        (+ acc (get result size)))
      0
      nm-sizes)))

(comment

  (calculate-part-one test-input)

  (calculate-part-one input)


  nil)

(defn remove-is-known [{:keys [correct]} set]
  (apply disj set (keys correct)))

(defn deduce-in-list [{:keys [correct]} xs]
  (mapv
    (fn [set]
      (reduce-kv
        (fn [set k v]
          (if (contains? set v)
            (-> set
              (disj v)
              (conj k))
            set))
        set
        correct))
    xs))

(defn deduce-in-known* [{:keys [correct]} m]
  (reduce-kv
    (fn [acc k v]
      (assoc acc k (reduce-kv
                     (fn [v k vv]
                       (if (contains? v vv)
                         (-> v
                           (disj vv)
                           (conj k))
                         v))
                     v
                     correct)))
    {}
    m))

;; =============================================================================
;; OMG part two!

(defn deduce-in-known [{:keys [known output] :as input}]
  (-> input
    (assoc :known (deduce-in-known* input known))
    (assoc :input (deduce-in-list input (:input input)))
    (assoc :output (deduce-in-list input output))))

(defn get-four [{:keys [known]}] (get known (get numbers 4)))
(defn get-eight [{:keys [known]}] (get known (get numbers 8)))
(defn get-one [{:keys [known]}] (get known (get numbers 1)))
(defn get-seven [{:keys [known]}] (get known (get numbers 7)))

(defn get-nine [{:keys [known input output] :as result}]
  (let [four (get-four result)]
    (->> (concat input output)
      (filter #(= (count %) 6))
      (filter #(set/subset? four %))
      first)))

(defn get-six [{:keys [known input output] :as result}]
  (let [one (get-one result)]
    (->> (concat input output)
      (filter #(= (count %) 6))
      (filter #(= 1 (count (set/difference one %))))
      first)))

(defn get-zero [{:keys [known input output] :as result}]
  (let [four (get-four result)
        six (get-six result)]
    (->> (concat input output)
      (filter #(= (count %) 6))
      (filter #(not (set/superset? % four)))
      (filter #(not= six %))
      first)))

(defn calculate-known [{:keys [input output]}]
  (reduce
    (fn [known x]
      (if-let [y (condp = (count x)
                   2 (get numbers 1)
                   4 (get numbers 4)
                   3 (get numbers 7)
                   7 (get numbers 8)
                   nil)]
        (assoc known y x)
        known))
    {}
    (concat input output)))

(defn calculate-e [{:keys [known] :as input}]
  (let [eight (get-eight input)
        nine (get-nine input)
        e (first (set/difference eight nine))]
    (-> input
      (assoc-in [:correct :E] e)
      deduce-in-known)))

(defn calculate-f [{:keys [known] :as input}]
  (let [six (get-six input)
        one (get-one input)
        e (first (set/intersection one six))]
    (-> input
      (assoc-in [:correct :F] e)
      deduce-in-known)))

(defn calculate-c [{:keys [correct] :as input}]
  (let [one (get-one input)
        x (apply disj one (keys correct))]
    (-> input
      (assoc-in [:correct :C] (first x))
      deduce-in-known)))

(defn calculate-d [{:keys [known] :as input}]
  (let [eight (get-eight input)
        zero (get-zero input)
        e (first (set/difference eight zero))]
    (-> input
      (assoc-in [:correct :D] e)
      deduce-in-known)))

(defn calculate-b [{:keys [known correct] :as input}]
  (let [four (get-four input)
        seven (get-seven input)
        four (disj four :D)
        e (first (set/difference four seven))]
    (-> input
      (assoc-in [:correct :B] e)
      deduce-in-known)))

(defn calculate-a [{:keys [known correct] :as input}]
  (let [seven (get-seven input)
        x (remove-is-known input seven)]
    (-> input
      (assoc-in [:correct :A] (first x))
      deduce-in-known)))

(defn calculate-g [{:keys [known input output correct] :as result}]
  (let [xs (concat input output)
        xs (reduce (fn [ac x] (apply conj ac x)) xs)
        x (remove-is-known result xs)]
    (-> result
      (assoc-in [:correct :G] (first x))
      deduce-in-known)))


(defn deduce [input]
  (let [known (calculate-known input)
        input (assoc input :known known)]
    (->
      input
      calculate-d
      calculate-e
      calculate-b
      calculate-f
      calculate-c
      calculate-a
      calculate-g)))

(defn replace-input-output [input]
  (let [{:keys [input output] :as result} (deduce input)]
    (assoc result :input (mapv nm input)
                  :output (mapv nm output))))

(comment

  (->> input
    (map replace-input-output)
    (map :output)
    (map str/join)
    (map parse-long)
    (apply +))

  nil)