(defn compress [s]
  (->> (reduce (fn [[[last-char n] & more :as accu] next-char]
                 (if (= last-char next-char)
                   (cons [last-char (inc n)] more)
                   (cons [next-char 1] accu)))
               '()
               s)
       (reverse)
       (map (fn [[c n]]
              (if (> n 1)
                (str c n)
                c)))
       (apply str)))

(println (compress (read-line)))
