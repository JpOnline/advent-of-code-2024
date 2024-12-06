(require '[clojure.string :as string])

;; Part 1

(def state
  {:guard-pos [6 4]
   :guard-dir [-1 0]
   :path #{}
   :board (string/split-lines
"....#.....
.........#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...")})

(def E [0 1])
(def S [1 0])
(def W [0 -1])
(def N [-1 0])

(do
  (def board (:board state))
  (def guard-pos (:guard-pos state))
  (def guard-dir (:guard-dir state)))

(defn move [{:keys [board guard-pos guard-dir path] :as state}]
  (let [next-pos (mapv + guard-pos guard-dir)
        turn {N E, E S, S W, W N}]
    (cond
      (= \. (get-in board next-pos)) (recur (-> state
                                             (assoc :guard-pos next-pos)
                                             (update :path conj guard-pos)))
      (= \# (get-in board next-pos)) (recur (assoc state :guard-dir (turn guard-dir)))
      :else path)))

(inc (count (distinct (move state))))

;; Part 2

(defn move [{:keys [board guard-pos guard-dir path] :as state}]
  (let [next-pos (mapv + guard-pos guard-dir)
        turn {N E, E S, S W, W N}]
    (cond
      (path [guard-pos guard-dir]) true
      (= \. (get-in board next-pos)) (recur (-> state
                                             (assoc :guard-pos next-pos)
                                             (update :path conj [guard-pos guard-dir])))
      (= \# (get-in board next-pos)) (recur (assoc state :guard-dir (turn guard-dir)))

      :else false)))

(defn put-obstruction [state [x y]]
  (update-in state [:board y] #(str (subs % 0 x) "#" (subs % (inc x) (count %)))))

(defn get-empty-spaces [{board :board :as state}]
  (for [y (range (count board))
        x (range (count (first board)))
        :when (and (= \. (get-in board [y x]))
                   (not= [y x] (:guard-pos state)))]
    [x y]))

(time 
  (->> (get-empty-spaces state)
    (map #(put-obstruction state %))
    (map move)
    (filter true?)
    (count)))
;="Elapsed time: 162291.600761 msecs"

(def state
  {:guard-pos [82 34]
   :guard-dir [-1 0]
   :path #{}
   :board (string/split-lines
"")})
