(ns aoc-2017-clj.day8
  (:gen-class)
  (:use aoc-2017-clj.util)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [instaparse.core :as insta :refer [defparser]]))

(def day8-grammar-res (io/resource "day8.bnf"))

(defparser day8-parser (slurp day8-grammar-res)
  :string-ci true
  :auto-whitespace :standard
  :output-format :enlive)

(defn op-gt []
  (fn [x y] (> x y)))
(defn op-get []
  (fn [x y] (>= x y)))

(defn op-lt []
  (fn [x y] (< x y)))
(defn op-let []
  (fn [x y] (<= x y)))

(defn op-eq []
  (fn [x y] (== x y)))
(defn op-neq []
  (fn [x y] (not (== x y))))

(defn day8-parse [str]
  (insta/transform
   {:stmtlist vector
    :id (comp str/join vector)
    :integer (comp string->int str/join vector)
    :relop identity
    :GT  op-gt
    :GET op-get
    :LT  op-lt
    :LET op-let
    :EQ  op-eq
    :NEQ op-neq
    :inc-stmt (comp
               #(assoc %1 :s-fn '+)
               #(zipmap [:s-var :s-val :c-var :c-fn :c-val] %1)
               vector)
    :dec-stmt (comp
               #(assoc %1 :s-fn '-)
               #(zipmap [:s-var :s-val :c-var :c-fn :c-val] %1)
               vector)
    }
   (day8-parser str)))

(defn request-var [m v]
  (or (m v)
      0))

(defn execute [str]
  (loop [program-list (first (day8-parse str))
        vars {:max 0}]
    (if (empty? program-list)
      vars
      (recur (next program-list)
             (let [stmt (first program-list)]
               (if ((:c-fn stmt) (request-var vars (:c-var stmt)) (:c-val stmt))
                 (let [new-val (eval (list (:s-fn stmt)
                                           (request-var vars (:s-var stmt))
                                           (:s-val stmt)))]
                   (assoc vars (:s-var stmt) new-val
                               :max (max (:max vars) new-val)))
                 vars))))))

(defn day8 [str]
  (let [vars (execute str)]
    {:max-val (apply max-key val (dissoc vars :max))
     :max-ever-val (:max vars)}))

(defn -main [& args]
  (println "day8" (day8 (slurp (io/resource "input-day8.txt")))))
