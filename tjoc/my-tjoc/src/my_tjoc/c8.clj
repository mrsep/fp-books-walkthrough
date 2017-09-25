(ns my-tjoc.c8
  (:require [clojure.walk :as walk]))

(require '[clojure.walk :as walk])

;;; chapter 8 - Macros

;; Quoting
(comment
  (def x 42)
  (def l '(23 42))
  '(1 2 3)
  `(1 2 3)
  `(1 2 3 ~x)
  `(1 2 3 ~l)
  `(1 2 3 ~@l)
  `('x `x x ~x)
  `(``x 1)
  `(''x 1)
  `(``~~~x `~~x)
  )

;; Code is Data is Code


; implement ->
; (-> 42 sqrt inc int) => (int (inc (sqrt 42)))
(defmacro thread-in [x f & fs]
  "basic implementation!
   Does not support:
    - tail recursion
    - sequences as f i.e. lambda functions"
  (let [fx `(~f ~x)]
    (if (seq fs)
      `(thread-in ~fx ~@fs)
      `~fx)))

; Warning: Never evaluate a form twice!
; evaluate it once, assign the result to a gensym symbol# and use that symbol

(comment
  (thread-in 14 inc inc float Math/sqrt)
  (macroexpand '(thread-in 14 inc inc float Math.sqrt))
  )

; contextual evaluation
(defn contextual-eval [context expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) context)]
      ~expr)))

(comment
  (contextual-eval {'a 1, 'b 2} '(+ a b))
  )

;; Defining control structures

(defmacro do-until [& clauses]
  (when clauses
    (list `when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException. "do-until requires an even number of forms!")))
          (cons 'do-until (nnext clauses)))))

(comment
  (do-until
   :truthy   (println "True.")
   (even? 0) (println "Even.")
   (odd?  1) (println "Odd.")
   (zero? 1) (println "Zero.")
   (pos?  1) (println "Pos."))

  (macroexpand '(do-until true (prn 1) false (prn 2)))
  )

(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))


;; Macros combining forms

(defmacro def-watched [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# " -> " new#)))))

(comment
  (def-watched x (* 12 12))
  x
  (def x 0)
  )

nil
