(load-file "eulerlib.clj")

(defn power5 [x] (int (Math/pow x 5)))

(defn digit-powered? [n] (= n (reduce + (map power5 (number-to-digitseq n)))))

(defn solve030 [] (reduce + (filter digit-powered? (range 2 10000000))))
