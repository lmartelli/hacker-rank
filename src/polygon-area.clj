(defn square [x]
  (* x x))

(defn sqrt [x]
  (Math/sqrt x))

(defn sub [[xa ya] [xb yb]]
  [(- xa xb) (- ya yb)])

(defn add [[xa ya] [xb yb]]
  [(+ xa xb) (+ ya yb)])

(defn norm [[x y]]
  (sqrt (+ (square x) (square y))))

(defn dist [a b]
  (norm (sub a b)))

(defn rotate-90 [[x y]]
  [y (- x)])

(defn prod [[xa ya] [xb yb]]
  (+ (* xa xb) (* ya yb)))

(defn signed-area [polygon]
  (if (= (count polygon) 3)
    (let [[p1 p2 p3] polygon
          u (sub p2 p1)
          v (sub p3 p1)
          b (norm u)
          h (/ (prod (rotate-90 u) v) b)]
      (/ (* b h) 2))
    (let [[a b c & more] polygon]
      (+ (signed-area [a b c])
         (signed-area (concat [a c] more))))))

(defn abs [x]
  (if (neg? x) (- x) ))

(defn area [polygon]
  (abs (signed-area polygon)))

;;

(defn vec-area [[x1 y1] [x2 y2]]
  (* 0.5
     (- (* x1 y2)
        (* x2 y1))))

(defn area [polygon]
  (->> (take (inc (count polygon)) (cycle polygon))
       (partition 2 1)
       (map #(apply vec-area %))))

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

(run-all-tests)
