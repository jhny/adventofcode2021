(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-row [s]
  (mapv keyword (str/split s #"-")))

(defn parse-input [file-name]
  (->> file-name
    io/resource
    slurp
    (str/split-lines)
    (mapv parse-row)))

(def test-input (parse-input "day12/test_input.txt"))
(def input (parse-input "day12/input.txt"))
(def example-input (parse-input "day12/example_input.txt"))
(def another-input (parse-input "day12/another_input.txt"))

(defn is-small [k]
  (re-matches #"[a-z]*" (name k)))

(defn starting-points [input]
  (->> input
    (filter (fn [x]
              (contains? (set x) :start)))
    (mapv (fn [x]
            (let [y (first (disj (set x) :start))
                  visited #{:start}]
              {:visited       visited
               :visited-twice nil
               :input         (set input)
               :current       y
               :path          [x]})))))

(defn has-visited [{:keys [visited]} new]
  (let [one (first new)
        two (second new)]
    (or
      (contains? visited one)
      (contains? visited two))))

(defn is-end [{:keys [current]}]
  (= current :end))

(defn follow-next [{:keys [input visited current] :as m}]
  (if (is-end m)
    [m]
    (->> (seq input)
      (filter (fn [x] (contains? x current)))
      (remove (fn [new] (has-visited m new)))
      (mapv (fn [x]
              (let [y (first (disj x current))
                    visited (if (is-small current)
                              (conj visited current)
                              visited)]
                (-> m
                  (assoc :current y)
                  (assoc :visited visited)
                  (update :path conj x))))))))

(defn calc-part-one [input]
  (let [result (loop [xs (starting-points input)]
                 (let [xs (mapcat follow-next xs)]
                   (if (every? is-end xs)
                     xs
                     (recur xs))))]
    (count result)))

(comment
  (calc-part-one test-input)

  (calc-part-one input)

  nil)

;; =============================================================================
;; part two

(defn disqualify [{:keys [visited visited-twice]} new]
  (let [one (first new)
        two (second new)
        has-visited (or
                      (contains? visited one)
                      (contains? visited two))]
    (cond

      (or (= :start one)
        (= :start two))
      true

      (nil? visited-twice)
      false

      :else
      (or
        has-visited
        (= visited-twice one)
        (= visited-twice two)))))

(defn follow-next2 [{:keys [input visited visited-twice current] :as m}]
  (if (is-end m)
    [m]
    (->> (seq input)
      (filter (fn [x] (contains? (set x) current)))
      (remove (fn [new] (disqualify m new)))
      (mapv (fn [x]
              (let [y (first (disj (set x) current))
                    is-small (is-small current)
                    visited-twice (cond
                                    visited-twice
                                    visited-twice

                                    (and is-small
                                      (contains? visited current))
                                    current)
                    visited (if is-small
                              (conj visited current)
                              visited)]
                (-> m
                  (assoc :current y)
                  (assoc :visited visited)
                  (assoc :visited-twice visited-twice)
                  (update :path conj x))))))))



(defn calc-part-two [input]
  (let [result (loop [xs (starting-points input)]
                 (let [xs (mapcat follow-next2 xs)]
                   (if (every? is-end xs)
                     xs
                     (recur xs))))]

    (count result)))


(comment
  (calc-part-two test-input)

  (calc-part-two example-input)

  (calc-part-two another-input)

  (calc-part-two input)

  nil)
