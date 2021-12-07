(ns day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.math :as math]))


(defn parse-input [file-name]
  (mapv parse-long (-> file-name
                     io/resource
                     slurp
                     (str/split #","))))

(def test-input (parse-input "test_input7.txt"))
(def input (parse-input "input7.txt"))

(defn distance [x goal]
  (math/abs (- x goal)))

(defn move [x-input x-output]
  (distance x-input x-output))

(defn calculate-part-one [input]
  (let [mi (apply min input)
        ma (apply max input)
        xs (range mi ma)]
    (->> xs
      (mapv (fn [x]
              [x (apply + (mapv #(move % x) input))]))
      (sort-by second)
      (first))))

(comment
  (calculate-part-one test-input)
  (calculate-part-one input)

  nil)

(defn move2 [x-input x-output]
  (reduce + 0 (range 1 (inc (distance x-input x-output)))))

(defn calculate-part-two [input]
  (let [mi (apply min input)
        ma (apply max input)
        xs (range mi ma)]
    (->> xs
      (mapv (fn [x]
              [x (apply + (mapv #(move2 % x) input))]))
      (sort-by second)
      (first))))

(comment

  (calculate-part-two test-input)

  (calculate-part-two input)

  nil)
