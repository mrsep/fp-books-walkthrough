(ns aoc-2017-clj.day1
  (:gen-class)
  (:use aoc-2017-clj.util)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn day1-uni [str offset-fn]
  (let [dv (string->digitvec str)
        cnt (count dv)
        offset (offset-fn dv)
        xform (comp (filter #(== (nth dv %)
                                 (nth dv (mod (+ % offset)
                                              cnt))))
                    (map #(nth dv %)))]
    (transduce xform + (range cnt))))

(defn day1-part1 [str]
  (day1-uni str (fn [& params] 1)))

(defn day1-part2 [str]
  (day1-uni str #(/ (count %) 2)))


(defn -main [& args]
  (println "day1:1" (day1-part1 (slurp (io/resource "input-day1.txt"))))
  (println "day1:2" (day1-part2 (slurp (io/resource "input-day1.txt"))))
  )
