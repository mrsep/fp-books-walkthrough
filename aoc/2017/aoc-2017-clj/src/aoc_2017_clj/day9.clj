(ns aoc-2017-clj.day9
  (:gen-class)
  (:use aoc-2017-clj.util)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [instaparse.core :as insta :refer [defparser]]))

(def day9-grammar-res (io/resource "day9.bnf"))

(defparser day9-parser (slurp day9-grammar-res)
  ;:string-ci true
  ;:auto-whitespace :standard
  :output-format :enlive)

(defn day9-parse [str]
  (insta/transform
   {    }
   (day9-parser str
                :total true
                :unhide :all
                )))

(defn day9 [str]
  (day9-parse str))

(defn -main [& args]
  (println "day9" (day9 (slurp (io/resource "test-input-day9.txt")))))
