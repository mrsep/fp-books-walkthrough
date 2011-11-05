(ns my-tjoc.c2)

;; functions

; overloading by arity
(defn make-set1
  "Takes either one or two arguments and builds a set of them."
  ([x]   #{x})
  ([x y] #{x y}))

; arbitrary arity
(defn make-set2
  "Takes an arbitrary number of arguments."
  ([x y & z] #{x y z}))

; define a var - more precise the root-binding of the var, the name is a symbol
(def x 42)

(.start (Thread. #(println "Answer: " x)))

; the do form defines a block of expressions
; all are evalled, the value of the last is returned
(do
  (def z 23)
  (println "Another answer:" z)
  (+ x 23))

;; loops and recursion
(defn print-down-from [x]
  (when (pos? x)
    (println x)
    (recur (dec x))))

(defn sum-down-from [init-x]
  (loop [sum 0, x init-x]
    (if (pos? x)
      (recur (+ sum x) (dec x))
      sum)))

;; quoting
'(this is a list)

; syntax quote for symbol auto-qualification
`map

; syntax quote for quote with unquotes
`(this is a list of ~(+ 5 1) words)

;; java interop
(new java.util.HashMap {"foo" 42 "bar" 23 "fizz" "bazz"})
(java.util.HashMap. {"foo" 42 "bar" 23 "fizz" "bazz"})

; calling instance methods
(.divide (java.math.BigDecimal. "42") 2M)

; set instance properties
(let [origin (java.awt.Point. 0 0)]
  (set! (.x origin) 15)
  (str origin))

; chain calling
(.. (java.util.Date.) toString (endsWith "2011"))
; instead of
(.endsWith (.toString (java.util.Date.)) "2010")

;; namespaces
*ns*

(clojure.set/intersection #{1 2 3} #{3 4 5})

; (ns ... (:use [... :only ... :exclude ...]))
(use 'clojure.set)

(intersection #{1 2 3} #{3 4 5})


nil
