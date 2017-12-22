(ns aoc-2017-clj.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn string->digitseq [str]
  (map #(Character/digit % 10) (seq str)))

(defn string->digitvec [str]
  (mapv #(Character/digit % 10) (seq str)))

(defn string->int [str]
  (Integer/parseInt (str/trim str)))

(defn string->integer [s]
  (when-let [d (re-find #"-?\d+" s)] (Integer. d)))
