(ns my-tjoc.c3)

;; Truethiness
(if true  :truthy :falsey)
(if false :truthy :falsey)
(if nil   :truthy :falsey)
(if 0     :truthy :falsey)
(if 1     :truthy :falsey)
(if []    :truthy :falsey)
(if ()    :truthy :falsey)
(if ""    :truthy :falsey)
(if (Boolean. "false")        :truthy :falsey) ; this is evil
(if Boolean/FALSE             :truthy :falsey)
(if (Boolean/valueOf "false") :truthy :falsey)
(if (seq []) :truthy :falsey)

; idiomatic loop over collections using the sequence abstraction
(defn print-seq [s]
  (when (seq s)
    (prn (first s))
    (recur (rest s))))

;; destructuring

; positional destructuring
(comment
  (let [[a b c & more :as all] (range 10)]
  (println "a b c is:" a b c)
  (println "more is: " more)
  (println "all is:  " all)))

(def my-name-map {:f-name "Hans" :m-name "Peter" :l-name "Peschke"})

(defn print-name [{:keys [f-name m-name l-name]}]
    (str l-name ", " f-name " " m-name))

;; the REPL

(defn colorize [f max-x max-y]
  (for [x (range max-x) y (range max-y)]
    [x y (mod (f x y) 256)]))

(defn methods-of [class]
  (for [method (seq (.getMethods class))
        :let [method-name (.getName method)]]
    method-name))

(comment
  (def frame (java.awt.Frame.)) )

; for f in {bit-xor, bit-and, bit-or, *}
(defn draw-f [frame f max-x max-y]
  (let [gfx (.getGraphics frame)]
    (.setSize frame (java.awt.Dimension. max-x max-y))
    (.show frame)
    (doseq [[x y c] (colorize f max-x max-y)]
      (.setColor gfx (java.awt.Color. c c c))
      (.fillRect gfx x y 1 1))))


nil
