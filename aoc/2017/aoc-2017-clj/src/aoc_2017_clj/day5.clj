(ns aoc-2017-clj.day5
  (:gen-class)
  (:use aoc-2017-clj.util)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn day5-part1 [str]
  (let [offset (mapv string->int (str/split-lines str))
        length (count offset)]
    (loop [memory offset
           index 0
           iter  0]
      (if (or (neg? index)
              (<= length index))
        iter
        (recur
         (assoc memory index (inc (nth memory index)))
         (+ index (nth memory index))
         (inc iter))))))

(defn day5-part2 [str]
  (let [offset (mapv string->int (str/split-lines str))
        length (count offset)]
    (loop [memory offset
           index 0
           iter  0]
      (if (or (neg? index)
              (<= length index))
        iter
        (let [current-offset (nth memory index)]
          (recur
           (assoc memory index (if (> current-offset 2)
                                 (dec current-offset)
                                 (inc current-offset)))
           (+ index current-offset)
           (inc iter)))))))

(defn -main [& args]
  (println "day5:1" (day5-part1 (slurp (io/resource "input-day5.txt"))))
  (println "day5:2" (day5-part2 (slurp (io/resource "input-day5.txt"))))
  )
