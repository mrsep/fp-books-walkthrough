(ns aoc-2017-clj.day7
  (:gen-class)
  (:use aoc-2017-clj.util)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [instaparse.core :as insta :refer [defparser]]))

(def day7-grammar-res (io/resource "day7.bnf"))

(defparser day7-parser (slurp day7-grammar-res)
  :string-ci true
  :auto-whitespace :standard
  :output-format :enlive)

(defn day7-parse [str]
  (insta/transform
   {:ID (comp str/join vector)
    :WEIGHT (comp string->int str/join vector)
    :PROGRAM vector}
   (day7-parser str)))

(defn children [node]
  (or (:children node)
      #{}))

(defn branch? [loc]
  (let [childs (children (zip/node loc))]
    (and (set? childs)
         (not (empty? childs)))))

(defn construct-node
  ([name] {:name name})
  ([name weight] {:name name
                  :weight weight})
  ([name weight childs] {:name name
                         :weight weight
                         :children (into #{} childs)}))

(defn make-node [node childs]
  (println node)
  (assoc node :children (into #{} childs)))

(defn circus-zip [root-node]
  (zip/zipper branch? children make-node root-node))

(defn construct-circus-zip-tree [root program-weights program-childs]
  (loop [loc (circus-zip (construct-node root (program-weights root)))]
    (if (:sum (zip/node loc))
      (if (zip/end? loc)
        loc
        (recur (zip/up loc)))
      (let [p-childs (program-childs (:name (zip/node loc)))
            z-childs (children (zip/node loc))
            diff     (clojure.set/difference p-childs z-childs)]
        (if (empty? diff)
          (zip/edit loc #(assoc (zip/node %1)
                                :sum (reduce + (map :sum %2))))
          (recur (zip/node (zip/down
                            (zip/make-node loc (construct-node (first diff))
                                           (program-weights (first diff)))))))))))


(defn day7 [str]
  (let [program-list (day7-parse str)
        program-weight (apply hash-map (flatten (map #(take 2 %) program-list)))
        program-childs (let [xform (comp (filter #(> (count %) 2))
                                         (map #(vector (first %) (into #{} (nnext %)))))]
                         (into (hash-map) (transduce xform conj program-list)))
        program-all    (into #{} (keys program-weight))
        non-root (reduce clojure.set/union (vals program-childs))
        root     (first (clojure.set/difference program-all non-root))
        ztree    (construct-circus-zip-tree root program-weight program-childs)]
    ztree))

(defn -main [& args]
  ;(println "day7"   (day7 (slurp (io/resource "test-input-day7.txt"))))
  (println "day7:2" "Not yet solved!")
  )
