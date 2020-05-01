(defn square [x]
  (* x x))

(defn sqrt [x]
  (Math/sqrt x))

(defn dist [[xa ya] [xb yb]]
  (sqrt (+ (square (- xa xb)) (square (- ya yb)))))

(defn rotate [coll]
  (->> coll
       cycle
       (drop 1)
       (take (count coll))))

(defn perimeter [points]
  (->> (map #(dist %1 %2) points (rotate points))
       (reduce +)))

;; RUN TESTS

(defn read-vect
  ([] (read-vect (read)))
  ([size] (mapv (fn [_] (read)) (range size))))

(defn read-items
  ([f] (read-items (read) f))
  ([n f] (take n (repeatedly f))))

(defn run-test []
  (println (perimeter (read-items #(read-vect 2)))))

(defn run-all-tests []
  (dotimes [nb-tests (read)]
    (run-test)))

(defn with [filename f]
  (with-open [is (java.io.PushbackReader.
                   (clojure.java.io/reader (str "/home/laurent/src/hackerrank/src/" filename)))]
    (binding [*in* is]
      (f))))

(run-test)
