(ns aoc-2017-clj.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [instaparse.core :as insta :refer [defparser]]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

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

(defn day5-part1 [str]
  (let [offset (mapv string->int (str/split-lines str))
        length (count offset)]
    (loop [memory offset
           index 0
           iter  0]
      (if (or (neg? index)
              (<= length index))
        iter
        (recur
         (assoc memory index (inc (nth memory index)))
         (+ index (nth memory index))
         (inc iter))))))

(defn day5-part2 [str]
  (let [offset (mapv string->int (str/split-lines str))
        length (count offset)]
    (loop [memory offset
           index 0
           iter  0]
      (if (or (neg? index)
              (<= length index))
        iter
        (let [current-offset (nth memory index)]
          (recur
           (assoc memory index (if (> current-offset 2)
                                 (dec current-offset)
                                 (inc current-offset)))
           (+ index current-offset)
           (inc iter)))))))

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
      {:cycles (count banks-set) :loop-length (- (count banks-set) (banks-cycle banks))}
      (recur (redistribute banks)
             (conj banks-set banks)
             (assoc banks-cycle banks (count banks-set))))))

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
  (println "day1:1" (day1-part1 (slurp (io/resource "input-day1.txt"))))
  (println "day1:2" (day1-part2 (slurp (io/resource "input-day1.txt"))))
  (println "day2:1" (day2-part1 (slurp (io/resource "input-day2.txt"))))
  (println "day2:2" (day2-part2 (slurp (io/resource "input-day2.txt"))))
  (println "day3:1" (day3-part1 265149))
  (println "day3:2" "Not yet solved!")
  (println "day4:1" (day4-part1 (slurp (io/resource "input-day4.txt"))))
  (println "day4:2" (day4-part2 (slurp (io/resource "input-day4.txt"))))
  (println "day5:1" (day5-part1 (slurp (io/resource "input-day5.txt"))))
  ;(println "day5:2" (day5-part2 (slurp (io/resource "input-day5.txt"))))
  (println "day5:2" "takes too long")
  (println "day6"   (day6 (slurp (io/resource "input-day6.txt"))))
  ;(println "day7"   (day7 (slurp (io/resource "test-input-day7.txt"))))
  (println "day7:2" "Not yet solved!")
  (println "day8"   (day8 (slurp (io/resource "input-day8.txt"))))
  )
