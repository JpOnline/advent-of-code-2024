(require '[clojure.string :as string])

;; Part 1

(def input
"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn get-nodes [board]
  (for [l (range (count board))
        c (range (count (first board)))
        :when (not= \. (get-in board [l c]))]
    [(get-in board [l c]) l c]))

(require '[clojure.math.combinatorics :as combo])

(defn get-antinodes [[_frequency nodes-pos]]
  (-> nodes-pos
    (combo/combinations 2)
    (->> (map (fn [[[f l1 c1] [_ l2 c2]]]
                (let [dif-l (- l1 l2)
                      dif-c (- c1 c2)
                      ant (str f "#")]
                  [[ant (+ l1 dif-l) (+ c1 dif-c)]
                   [ant (- l2 dif-l) (- c2 dif-c)]]))))))

(defn in-bound? [input [_ l c]]
  (not (nil? (get-in (string/split-lines input) [l c]))))

(->> input
  (string/split-lines)
  (get-nodes)
  (group-by first)
  ;; (second))
  (map get-antinodes)
  (flatten)
  (partition 3)
  (filter #(in-bound? input %))
  (map (juxt second #(nth % 2)))
  (set)
  (count))

;; Part 2

(defn next-antinode [[[f l1 c1] [_ l2 c2]]]
  (let [dif-l (- l2 l1)
        dif-c (- c2 c1)
        ant (str f "#")]
    [[f l2 c2] [ant (+ l2 dif-l) (+ c2 dif-c)]]))

(defn get-antinodes-2 [input [_frequency nodes-pos]]
  (-> nodes-pos
    (combo/combinations 2)
    (->> (map (fn [[n1 n2]]
                (concat
                  (map second (take-while #(in-bound? input (second %)) (iterate next-antinode [n1 n2])))
                  (map second (take-while #(in-bound? input (second %)) (iterate next-antinode [n2 n1])))))))))

(->> input
  (string/split-lines)
  (get-nodes)
  (group-by first)
  ;; (second)
  (map #(get-antinodes-2 input %))
  (flatten)
  (partition 3)
  (filter #(in-bound? input %))
  (map (juxt second #(nth % 2)))
  (set)
  (count))
