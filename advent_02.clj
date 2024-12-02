(def reports "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(require '[clojure.string :as string])

(defn parse-line [line]
  (->> line
    (re-seq #"\d+")
    (mapv #(Integer/parseInt %))))

;; Part 1

(defn increasing? [line]
  (->> line
    (partition 2 1)
    (every? (fn [[a b]] (> a b)))))

(defn deacreasing? [line]
  (->> line
    (partition 2 1)
    (every? (fn [[a b]] (< a b)))))

(defn difference-1-to-3? [line]
  (->> line
    (partition 2 1)
    (every? (fn [[a b]] (and (>= (abs (- a b)) 1)
                             (<= (abs (- a b)) 3))))))

(->> reports
  (string/split-lines)
  (map parse-line)
  (map #(and (or (deacreasing? %) (increasing? %))
             (difference-1-to-3? %)))
  (filter true?)
  (count))

;; Part 2
(defn inc-difference-1-to-3? [line]
  (->> line
    (partition 2 1)
    (map (fn [[a b]] (and (>= (abs (- a b)) 1)
                          (<= (abs (- a b)) 3)
                          (> a b))))
    (filter false?)
    (count)))

(defn dec-difference-1-to-3? [line]
  (->> line
    (partition 2 1)
    (map (fn [[a b]] (and (>= (abs (- a b)) 1)
                          (<= (abs (- a b)) 3)
                          (< a b))))
    (filter false?)
    (count)))

(defn get-pred
  "Returns the first element of coll that satisfies the predicate f."
  [f coll]
  (some #(when (f %) %) coll))

(defn drop-at [n coll]
  (let [[left right] (split-at n coll)]
    (vec (concat left (rest right)))))

(get-pred odd? [2 4 6])

(defn remove-1-and-check [line]
  (->> line
    (count)
    (range)
    (get-pred #(->> line
                 (drop-at %)
                 ((fn [changed-line] (zero? (min (inc-difference-1-to-3? changed-line) (dec-difference-1-to-3? changed-line)))))))
    (nil?)
    (not)))

(->> reports
  (string/split-lines)
  (map parse-line)
  (map #(into [ (inc-difference-1-to-3? %) (dec-difference-1-to-3? %)])))

(->> reports
  (string/split-lines)
  (map parse-line)
  (map #(if (zero? (min (inc-difference-1-to-3? %) (dec-difference-1-to-3? %)))
          true
          (remove-1-and-check %)))
  (remove false?)
  (count))
