(ns aoc-2017-clj.day3
  (:gen-class)
  (:use aoc-2017-clj.util)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

; k=0 ring has 1 element
; kth ring has 4*2*k elements
; smallest number on kth ring: 1+(2*k-1)^2
; biggest number on kth ring: (2*k+1)^2
; diagonals: biggest number - {0,1,2,3}*2*k
; path length from diagonal to axes: k

; Given: a number n >= 1
; find k such that: 1+(2*k-1) ^ 2 <= n <= (2*k+1) ^ 2
; k <= |_(sqrt(n-1)+1)/2_| and /(sqrt(n) - 1)/2\ <= k
; find distance from diagonal

(defn getPathLength [n]
  (let [k (Math/ceil (/ (dec (Math/sqrt n)) 2))
        big (Math/pow (inc (* 2 k)) 2)
        small (inc(Math/pow (dec (* 2 k))2))
        cnt (* 8 k)
        diags (mapv #(- big (* 2 k %)) [0 1 2 3])
        diags_dist (map #(Math/abs (- n %)) (conj diags (dec small)))
        small_diag_dist (apply min diags_dist)]
    (- (* 2 k) small_diag_dist)))

(defn day3-part1 [n]
  (getPathLength n))

(defn -main [& args]
  (println "day3:1" (day3-part1 265149))
  (println "day3:2" "Not yet solved!")
  )
