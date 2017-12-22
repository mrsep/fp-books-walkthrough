(ns aoc-2017-clj.day6
  (:gen-class)
  (:use aoc-2017-clj.util)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn redistribute [banks]
  (let [indexed-banks (into (sorted-map-by >) (zipmap (range (count banks)) banks))
        max-bank (key (apply max-key val indexed-banks))
        max-val (nth banks max-bank)]
    (loop [new-banks (assoc banks max-bank 0)
           remaining max-val
           index (mod (inc max-bank)
                      (count banks))]
      (if (zero? remaining)
        new-banks
        (recur (update new-banks index inc)
               (dec remaining)
               (mod (inc index)
                    (count banks)))))))

(defn day6 [str]
  (loop [banks       (mapv string->int (str/split str #"\s"))
         banks-set   #{}
         banks-cycle {}]
    (if (banks-set banks)
      {:cycles (count banks-set)
       :loop-length (- (count banks-set) (banks-cycle banks))}
      (recur (redistribute banks)
             (conj banks-set banks)
             (assoc banks-cycle banks (count banks-set))))))

(defn -main [& args]
  (println "day6"   (day6 (slurp (io/resource "input-day6.txt")))))
