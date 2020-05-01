(defn is-function [pairs]
  (->> pairs
       (map first)
       frequencies
       vals
       (filter #(> % 1))
       empty?))

(defn read-vect
  ([] (read-vect (read)))
  ([size] (mapv (fn [_] (read)) (range size))))

(defn read-items
  ([f] (read-items (read) f))
  ([n f] (take n (repeatedly f))))

(defn run-test []
  (println
    (if (is-function (read-items #(read-vect 2)))
      "YES"
      "NO")))

(defn main []
  (dotimes [nb-tests (read)]
    (run-test)))

(defn with [filename f]
  (with-open [is (java.io.PushbackReader.
                   (clojure.java.io/reader (str "/home/laurent/src/hackerrank/src/" filename)))]
    (binding [*in* is]
      (f))))

(main)
