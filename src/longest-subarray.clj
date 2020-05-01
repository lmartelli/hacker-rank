(ns hackerrank.longestsubarray)

(defn subarray-length [arr start]
  (loop [end start
         curr-min (get arr start)
         curr-max curr-min]
    (cond (> (- curr-max curr-min) 1) (- end start)
          (= end (dec (count arr))) (inc (- end start))
          :else (let [next-val (get arr (inc end))]
                  (recur (inc end) (min curr-min next-val) (max curr-max next-val))))))

(defn longestSubarray [arr]
  (->> (map #(subarray-length arr %) (range (count arr)))
       (apply max)))
