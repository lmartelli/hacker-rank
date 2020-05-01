(ns hackerrank.gcdsubseq)

(def primes
  (loop [p [2]
         remaining (range 3 1000 2)]
    (if (empty? remaining)
      p
      (let [next-prime (first remaining)]
      (recur
        (conj p next-prime)
        (filterv #(pos? (mod % next-prime)) remaining))))))

(defn divides? [n m]
  (zero? (mod m n)))

(def divisors
  (memoize
    (fn [n]
        (if (= 1 n)
          [1]
          (let [p (->> primes
                       (filter #(divides? % n))
                       first)]
            (conj (divisors (/ n p)) p))))))

(defn gcd [nums]
  (->>
    (reduce
      (fn [factors1 factors2]
        (into {}
              (map
                (fn [[factor n]]
                  [factor (min n (get factors2 factor))])
                (select-keys factors1 (keys factors2)))))
      (map (comp frequencies divisors) nums))
    (mapcat (fn [[factor n]] (repeat n factor)))
    (reduce *)))

(defn tails [items]
  (when (not-empty items)
    (lazy-seq (cons items (tails (rest items))))))

(defn subseqs [coll k]
  (if (zero? k)
    [[]]
    (mapcat (fn [[head & tail]]
              (map #(cons head %) (subseqs tail (dec k))))
         (drop-last (dec k) (tails coll)))))

(defn greatest-gcd-subseq [coll k]
  (->> (subseqs coll k)
       (apply max-key gcd)))
