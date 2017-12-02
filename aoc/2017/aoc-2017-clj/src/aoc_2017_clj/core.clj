(ns aoc-2017-clj.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(defn string->digitseq [str]
  (map #(Character/digit % 10) (seq str)))

(defn string->digitvec [str]
  (mapv #(Character/digit % 10) (seq str)))

(defn day1-part1 [str]
  "still too ugly!"
  (let [digit-seq (string->digitseq str)]
    (if-not (and (first digit-seq)
                 (second digit-seq))
      0
      (loop [rst     digit-seq
             current (first digit-seq)
             nxt     (second digit-seq)
             sum     0]
        (if-not nxt
          (if (== current (first digit-seq))
            (+ sum current)
            sum)
          (recur (rest rst)
                 (second rst)
                 (second (next rst))
                 (if (== current nxt)
                   (+ sum current)
                   sum)))))))

(defn day1-part2 [str]
  (let [dv (string->digitvec str)
        cnt (count dv)
        halflen (/ cnt 2)
        xform (comp (map #(vector (nth dv %)
                                  (nth dv (mod (+ % halflen) cnt))))
                    (filter #(== (nth % 0) (nth % 1)))
                    (map first))]
    (transduce xform + (range cnt))))

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
