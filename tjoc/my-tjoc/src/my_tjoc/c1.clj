(ns my-tjoc.c1)

(defn hello-world [name] (clojure.core/println "Hello" name ":)"))

(defprotocol Concatenatable
  (cat [this other]))

(extend-type String
  Concatenatable
  (cat [this other]
    (.concat this other)))

(extend-type java.util.List
  Concatenatable
  (cat [this other]
    (concat this other)))

