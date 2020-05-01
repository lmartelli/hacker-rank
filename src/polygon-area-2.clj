(defn vec-area [[x1 y1] [x2 y2]]
  (- (* x1 y2)
     (* x2 y1)))

(defn abs [x]
  (if (neg? x)
    (- x)
    x))

(defn area [polygon]
  (->> (map vec-area polygon (rest (cycle polygon)))
       (reduce +')
       (* 0.5M)
       (abs)))

;; RUN TESTS

(defn read-vect
  ([] (read-vect (read)))
  ([size] (mapv (fn [_] (read)) (range size))))

(defn read-items
  ([f] (read-items (read) f))
  ([n f] (take n (repeatedly f))))

(defn run-test []
  (println (area (read-items #(read-vect 2)))))

(defn with [filename f]
  (with-open [is (java.io.PushbackReader.
                   (clojure.java.io/reader (str "/home/laurent/src/hackerrank/src/" filename)))]
    (binding [*in* is]
      (f))))

(defn run-all-tests
  ([] (run-test))
  ([filename] (with filename #(run-all-tests))))

;;(run-all-tests)

;; DATA GEN

(defn points
  ([] (letfn [(rand [] (rand-int Integer/MAX_VALUE))]
        (repeatedly #(vector (rand) (rand)))))
  ([n] (take n (points))))
