(ns day6
  (:require [clojure.string :as str]))



(defn parse-input [s]
  (mapv parse-long (str/split s #",")))

(def test-input (parse-input "3,4,3,1,2"))
(def input (parse-input "1,1,3,5,3,1,1,4,1,1,5,2,4,3,1,1,3,1,1,5,5,1,3,2,5,4,1,1,5,1,4,2,1,4,2,1,4,4,1,5,1,4,4,1,1,5,1,5,1,5,1,1,1,5,1,2,5,1,1,3,2,2,2,1,4,1,1,2,4,1,3,1,2,1,3,5,2,3,5,1,1,4,3,3,5,1,5,3,1,2,3,4,1,1,5,4,1,3,4,4,1,2,4,4,1,1,3,5,3,1,2,2,5,1,4,1,3,3,3,3,1,1,2,1,5,3,4,5,1,5,2,5,3,2,1,4,2,1,1,1,4,1,2,1,2,2,4,5,5,5,4,1,4,1,4,2,3,2,3,1,1,2,3,1,1,1,5,2,2,5,3,1,4,1,2,1,1,5,3,1,4,5,1,4,2,1,1,5,1,5,4,1,5,5,2,3,1,3,5,1,1,1,1,3,1,1,4,1,5,2,1,1,3,5,1,1,4,2,1,2,5,2,5,1,1,1,2,3,5,5,1,4,3,2,2,3,2,1,1,4,1,3,5,2,3,1,1,5,1,3,5,1,1,5,5,3,1,3,3,1,2,3,1,5,1,3,2,1,3,1,1,2,3,5,3,5,5,4,3,1,5,1,1,2,3,2,2,1,1,2,1,4,1,2,3,3,3,1,3,5"))

(defn step [xs index]
  (let [interval-timer (nth xs index)]
    (if (zero? interval-timer)
      (conj (assoc xs index 6) 8)
      (update xs index dec))))

(defn step-a-day [xs _]
  (reduce step xs (range (count xs))))

(comment

  (count (reduce step-a-day test-input (range 80)))

  (count (reduce step-a-day input (range 80)))

  nil)

;; =============================================================================
;; part two, Yep part one is too slow :(

(def days [0 1 2 3 4 5 6 7 8])

(defn fix-input [input]
  (let [input (frequencies input)]
    (reduce
      (fn [acc day]
        (if (contains? acc day)
          acc
          (assoc acc day 0)))
      input
      days)))

(defn step-single-day [input _]
  (reduce
    (fn [acc day]
      (let [fish (get input day)]
        (if (zero? day)
          (-> acc
            (update day #(- % fish))
            (update 6 #(+ % fish))
            (update 8 #(+ % fish)))
          (-> acc
            (update day #(- % fish))
            (update (dec day) #(+ % fish))))))
    input
    days))

(defn count-lanternfish [input days]
  (apply + (vals (reduce step-single-day (fix-input input) (range days)))))

(comment

  (count-lanternfish test-input 256)

  (count-lanternfish input 256)

  nil)


