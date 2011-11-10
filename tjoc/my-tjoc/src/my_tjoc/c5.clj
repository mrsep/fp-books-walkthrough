(ns my-tjoc.c5)

;; Persistence, sequences and complexity

; Java Arrays are not persistent
(comment
  (def ds (into-array [:willie :barnabas :adam]))
  (seq ds)
  (aset ds 1 :quentin)
  )

; primitive vectors - vec of Java's primitive types
(into (vector-of :int) [Math/PI 2 1.3])

; nested vectors (matrices) come 
; along with {get-in, update-in, assoc-in}

(def matrix
  [[1 2 3]
   [4 5 6]
   [7 8 9]])


(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
     (filter (fn [new-yx]
               (every? #(< -1 % size) new-yx))
             (map #(map + yx %) deltas))))

(map #(get-in matrix %) (neighbors 3 [0 0]))

; subvectors can be nested and are actually not nested
; how to do this on nested vectors (subarrays/submatrices)???


(defn pos [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))

(defn index [coll]
  (cond
   (map? coll) (seq coll)
   (set? coll) (map vector coll coll)
   :else (map vector (iterate inc 0) coll)))


 nil