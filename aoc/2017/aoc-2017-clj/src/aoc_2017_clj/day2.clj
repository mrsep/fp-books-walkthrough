(ns aoc-2017-clj.day2
  (:gen-class)
  (:use aoc-2017-clj.util)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(defn day2-part1 [str]
  (let [rows (map #(str/split % #" ") (str/split-lines str))
        row-xform (comp (map string->integer)
                        (filter number?))
        xform (comp (map #(into (sorted-set) row-xform %))
                    (map #(- (last %) (first %))))]
    (transduce xform + rows)))

(defn evenly-dividable? [[a b]]
  (zero? (mod (max a b)
              (min a b))))

(defn evenly-divide [[a b]]
  (/ (max a b) (min a b)))

(defn day2-part2 [str]
  (let [rows (map #(str/split % #" ") (str/split-lines str))
        row-xform1 (comp (map string->integer)
                         (filter number?))
        xform (comp (map #(into [] row-xform1 %))
                    (map #(combo/cartesian-product % %)))
        all (transduce xform conj rows)
        row-xform2 (comp (filter #(not= (first %) (second %)))
                         (filter evenly-dividable?)
                         (map evenly-divide)
                         (take 1))
        final (map #(transduce row-xform2 + %) all)]
    (reduce + final)))

(defn -main [& args]
  (println "day2:1" (day2-part1 (slurp (io/resource "input-day2.txt"))))
  (println "day2:2" (day2-part2 (slurp (io/resource "input-day2.txt"))))
  )
