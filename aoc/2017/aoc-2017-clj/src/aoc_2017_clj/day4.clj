(ns aoc-2017-clj.day4
  (:gen-class)
  (:use aoc-2017-clj.util)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn day4-part1 [str]
  (let [phrases-vec (mapv #(str/split % #" ") (str/split-lines str))
        phrases-set (map #(into #{} %) phrases-vec)
        valid (map #(== (count %1) (count %2)) phrases-vec phrases-set)]
    (count (filter identity valid))))

(defn string->charset [str]
  (into #{} (seq str)))

(defn day4-part2 [str]
  (let [phrases-vec (mapv #(map string->charset (str/split % #" ")) (str/split-lines str))
        phrases-set (map #(into #{} %) phrases-vec)
        valid (map #(== (count %1) (count %2)) phrases-vec phrases-set)]
    (count (filter identity valid))))

(defn -main [& args]
  (println "day4:1" (day4-part1 (slurp (io/resource "input-day4.txt"))))
  (println "day4:2" (day4-part2 (slurp (io/resource "input-day4.txt"))))
  )
