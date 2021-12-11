(ns day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-row [s]
  (mapv (comp parse-long str) s))
(defn parse-input [file-name]
  (->> file-name
    io/resource
    slurp
    (str/split-lines)
    (mapv parse-row)))

(def test-input (parse-input "day11/test_input.txt"))
(def input (parse-input "day11/input.txt"))
(def example-input (parse-input "day11/example_input.txt"))

(defn get-from-coord [grid [x y]]
  (try (nth (nth grid y) x)
       (catch Exception _ nil)))

(defn increase [grid [x y]]
  (try (update grid y (fn [xs] (update xs x inc)))
       (catch Exception _ grid)))

(defn set-to-zero [grid [x y]]
  (try (update grid y (fn [xs] (assoc xs x 0)))
       (catch Exception _ grid)))

(defn all-adjecent [[x y]]
  (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]
    [(inc x) (inc y)] [(dec x) (dec y)] [(inc x) (dec y)] [(dec x) (inc y)]))


(defn increase-and-all-adjecent [grid [x y :as coord] flashed?]

  (reduce
    (fn [[flashed? grid :as acc] [x y]]
      (cond

        (flashed? [x y])
        [flashed? grid]

        (and
          (not (flashed? [x y]))
          (= 9 (get-from-coord grid [x y])))
        (let [flashed? (conj flashed? [x y])
              grid (set-to-zero grid [x y])]
          (increase-and-all-adjecent grid [x y] flashed?))

        :else
        [flashed? (increase grid [x y])]))
    [flashed? grid]
    (all-adjecent coord)))

(defn do-single-step [input]
  (reduce
    (fn [[flashed? grid :as acc] y]
      (reduce
        (fn [[flashed? grid :as acc] x]
          (cond

            (flashed? [x y])
            [flashed? grid]

            (and
              (not (flashed? [x y]))
              (= 9 (get-from-coord grid [x y])))
            (let [flashed? (conj flashed? [x y])
                  grid (set-to-zero grid [x y])]
              (increase-and-all-adjecent grid [x y] flashed?))

            :else
            [flashed? (increase grid [x y])]))


        acc
        (range (count (nth grid y)))))
    [#{} input]
    (range (count input))))

(defn calc-flashes [input steps]
  (reduce
    (fn [[input flashes] _]
      (let [[flashes? input :as y] (do-single-step input)]
        [input (+ flashes (count flashes?))]))
    [input 0]
    (range steps)))

;; =============================================================================
;; Part two

(defn get-count-input [input]
  (* (count (first input)) (count input)))

(defn calc-all-flashes [input]
  (let [input-count (get-count-input input)]
    (loop [input input
           step 1]
      (if (int? input)
        input
        (let [[flashes? input :as y] (do-single-step input)]
          (if (= (count flashes?) input-count)
            (recur step step)
            (recur input (inc step))))))))

(comment

  (calc-all-flashes test-input)

  (calc-all-flashes input)

  nil)
