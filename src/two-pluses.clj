(ns hackerrank.twopluses)

(require '[clojure.test :refer :all])

(defn plus-area [arm-length]
  (+ 1 (* 4 arm-length)))

(defn width [grid]
  (count (first grid)))

(defn height [grid]
  (count grid))

(defn max-plus-size [grid]
  (-> (let [minimum (min (width grid) (height grid))]
        (if (odd? minimum)
          minimum
          (dec minimum)))
      (quot 2)))

(defn get-cell [grid [x y]]
  (get-in grid [y x]))

(defn add [[xa ya] [xb yb]]
  [(+ xa xb) (+ ya yb)])

(defn plus-cells [[size pos]]
  (cons
    pos
    (mapcat (fn [offset]
              (map #(add pos %) [[0 offset] [0 (- offset)] [offset 0] [(- offset) 0]]))
            (range 1 (inc size)))))

(defn plus? [grid size pos]
  (->> (plus-cells [size pos])
       (map #(get-cell grid %))
       (every? #{\G})))

(defn tails [items]
  (when (not-empty items)
    (lazy-seq (cons items (tails (rest items))))))

(defn pairs [items]
  (mapcat (fn [items] (map #(vector (first items) %) (rest items))) (tails items)))

(defn pluses 
  ([grid]
   (map
     (fn [size] (vector size (pluses grid size)))
     (range (max-plus-size grid) -1 -1)))
  ([grid size]
   (->> (for [x (range size (- (width grid) size))
              y (range size (- (height grid) size))]
          [x y])
        (filter #(plus? grid size %))
        set)))

(defn overlap? [pluses]
  (not-empty
    (apply filter
           (map (comp set plus-cells) pluses))))

(defn area-product [pluses]
  (->> pluses
       (map (comp plus-area first))
       (reduce *)))

(defn twoPluses [grid]
  (->> (pluses grid)
       (mapcat (fn [[size positions]] (map #(vector size %) positions)))
       (pairs)
       (remove overlap?)
       (map area-product)
       (apply max)))

;; tests

(deftest plus-area-test
  (are [l area] (= area (plus-area l))
    0 1
    1 5
    2 9
    3 13))

(deftest plus?-test
  (let [grid ["GGGGGG"
              "GBBBGB"
              "GGGGGG"
              "GGBBGB"
              "GGGGGG"]]
    (are [size x y] (plus? grid size [x y])
      0 0 0
      0 0 1
      0 1 0
      0 2 2
      1 4 2)))

(deftest pluses-with-size-test
  (let [grid ["GGGGGG"
              "GBGBGB"
              "GGGGGG"
              "GGGBGB"
              "GGGGGG"]]
    (are [size plus-positions] (= plus-positions (pluses grid size))
      1 #{[1 3] [4 2] [2 2]}
      2 #{[2 2]})))

(deftest all-pluses-test
  (let [grid ["BBGBBB"
              "GBGBGB"
              "GGGGGG"
              "BBGBGB"
              "BBGGBB"]]
    (is (= [[2 #{[2 2]}]
            [1 #{[2 2] [4 2]}]
            [0 #{[2 0]
                 [0 1] [2 1] [4 1]
                 [0 2] [1 2] [2 2] [3 2] [4 2] [5 2]
                 [2 3] [4 3]
                 [2 4] [3 4]}]]
           (pluses grid)))))

(deftest plus-cells-test
  (are [size pos cells] (= cells (set (plus-cells [size pos])))
    0 [3 4] #{[3 4]}
    1 [3 4] #{[3 4] [3 5] [3 3] [2 4] [4 4]}
    2 [2 2] #{[2 2] [2 3] [2 4] [2 1] [2 0] [1 2] [0 2] [3 2] [4 2]}))
