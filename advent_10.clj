(require '[clojure.string :as string])

;; Part 1

(def trail-map*
"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(def trail-map*
"10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01")

(def E [0 1])
(def S [1 0])
(def W [0 -1])
(def N [-1 0])

(defn get-neighboars [pos]
  (map #(mapv + pos %) [N W S E]))

(defn valid-next-step? [trail-map required-height]
  (fn [pos]
    (= required-height (get-in trail-map pos))))

(defn grow-path [trail-map height]
  (fn [trail-path]
    (->> trail-path
      (last)
      (get-neighboars)
      (filter (valid-next-step? trail-map (inc height)))
      (map #(conj trail-path %)))))


(defn n-trail-paths [trail-map trail-head]
  (->> (reduce #(mapcat (grow-path trail-map %2) %1) [[trail-head]] (range 9))
    ;; For part 2 I just needed to comment the next line
    ;; (group-by last)
    count))

(defn get-trail-heads [trail-map]
  (for [l (range (count trail-map))
        c (range (count (first trail-map)))
        :when (= 0 (get-in trail-map [l c]))]
    [l c]))

(let [trail-map (->> trail-map*
                  (string/split-lines)
                  (mapv (fn [l] (mapv #(parse-long (str %)) l))))
      trail-heads (get-trail-heads trail-map)]
  (->> trail-heads
    (map #(n-trail-paths trail-map %))
    (reduce +)))
