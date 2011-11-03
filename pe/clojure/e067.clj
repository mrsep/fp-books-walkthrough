(import java.io.FileReader java.io.BufferedReader)

(def data (line-seq (new BufferedReader (new FileReader "e067.txt"))))
(defn str2int [s] (Integer/valueOf s))

(def data (map #(map str2int (re-seq #"\d\d" %)) data))

(def data (vec (map vec data)))

;;(defn dat [level element] ((data level) element))
(defn dat [tree [level element]] (if (and
                                       (< level (count tree)) 
                                       (<= element level)
                                       (>= level 0)
                                       (>= element 0))
                                   ((tree level) element)
                                   []))


(defn lchild [level element] [(inc level) element])
(defn rchild [level element] [(inc level) (inc element)])

(defn childvalues [tree level element] [(dat tree (lchild level element)) 
                                        (dat tree (rchild level element))])

(defn lparent [level element] [(dec level) (dec element)])
(defn rparent [level element] [(dec level) element])

(defn parentvalues [tree level element] [(dat tree (lparent level element))
                                         (dat tree (rparent level element))])

(defn settree [tree level element new] (assoc tree level 
                                              (assoc (tree level) element new)))

(defn solvelastlevel [tree] (loop [t tree, l (- (count tree) 2), e 0]
                              (if (<= e l)
                                (recur 
                                  (settree t l e (+ (dat t [l e])
                                                    (apply max (childvalues t l e))))
                                  l 
                                  (inc e))
                                (subvec t 0 (inc l)))))

(defn solve067 [] (loop [t data, l (count data)]
                    (if (> l 1)
                      (recur (solvelastlevel t) (dec l))
                      t)))
