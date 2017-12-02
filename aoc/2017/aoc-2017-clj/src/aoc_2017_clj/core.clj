(ns aoc-2017-clj.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(defn string->digitseq [str]
  (map #(Character/digit % 10) (seq str)))

(defn string->digitvec [str]
  (mapv #(Character/digit % 10) (seq str)))

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

(defn string->int [str]
  (Integer/parseInt (str/trim str)))

(defn string->integer [s]
  (when-let [d (re-find #"-?\d+" s)] (Integer. d)))

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
  (println "day1:1" (day1-part1 (slurp (io/resource "input-day1.txt"))))
  (println "day1:2" (day1-part2 (slurp (io/resource "input-day1.txt"))))
  (println "day2:1" (day2-part1 (slurp (io/resource "input-day2.txt"))))
  (println "day2:2" (day2-part2 (slurp (io/resource "input-day2.txt"))))
  )
