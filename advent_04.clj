(require '[clojure.string :as string])

;; Part 1

(def input
"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def board (string/split-lines input))

;; Part 1
(defn find-xmas [board pos]
  (let [dir-positions (fn [dir] (map #(mapv + pos %) dir))
        right [[0 0] [0 1] [0 2] [0 3]]
        left [[0 0] [0 -1] [0 -2] [0 -3]]
        up [[0 0] [-1 0] [-2 0] [-3 0]]
        down [[0 0] [1 0] [2 0] [3 0]]
        se [[0 0] [1 1] [2 2] [3 3]]
        sw [[0 0] [1 -1] [2 -2] [3 -3]]
        ne [[0 0] [-1 1] [-2 2] [-3 3]]
        nw [[0 0] [-1 -1] [-2 -2] [-3 -3]]
        is-xmas? (fn [dir] (= "XMAS" (apply str (map #(get-in board %) (dir-positions dir)))))]
    (->> [right left up down se sw ne nw]
      (map is-xmas?)
      (filter true?)
      (count))))

(reduce +
  (for [x (range (count board))
        y (range (count (first board)))
        :when (= \X (get-in board [x y]))]
    (find-xmas board [x y])))

;; Part 2
(defn find-x-mas [board pos]
  (let [dir-positions (fn [dir] (map #(mapv + pos %) dir))
        se [[-1 -1] [0 0] [1 1]]
        sw [[-1 1] [0 0] [1 -1]]
        ne [[1 -1] [0 0] [-1 1]]
        nw [[1 1] [0 0] [-1 -1]]
        is-mas? (fn [dir] (= "MAS" (apply str (map #(get-in board %) (dir-positions dir)))))]
    (->> [se sw ne nw]
      (map is-mas?)
      (filter true?)
      (count))))

(count 
  (filter #(= 2 %)
    (for [x (range (count board))
          y (range (count (first board)))
          :when (= \A (get-in board [x y]))]
      (find-x-mas board [x y]))))
