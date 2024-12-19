(def bytes [5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0])

(def MAX-POS 6)
(def fallen-bytes 12)

(defn corrupted-space [bytes n]
  (take n (partition 2 bytes)))

(def D [0 1])
(def R [1 0])
(def U [0 -1])
(def L [-1 0])

(defn get-neighboars [pos]
  (map #(mapv + pos %) [R D U L]))

(defn valid-next-step? [corrupted visited]
  (fn [pos]
    (and
      (> (apply min pos) -1)
      (<= (apply max pos) MAX-POS)
      (not (contains? (clojure.set/union (set corrupted) (set visited)) pos)))))

(defn grow-path [corrupted]
  (fn [trail-path]
    (->> trail-path
      (last)
      (get-neighboars)
      (filter (valid-next-step? corrupted trail-path))
      (map #(conj trail-path %)))))

;; Failed attempt.. takes too long.
(let [corrupted (corrupted-space bytes fallen-bytes)]
  (loop [last-positions [[0 0]]
         visited #{}
         counter 0]
    (let [valid-neighbors (filter (valid-next-step? corrupted visited) (mapcat get-neighboars last-positions))
          new-visited (apply conj visited last-positions)]
      (cond
        (> counter 5000) last-positions

        (some #(= [MAX-POS MAX-POS] %) last-positions) counter
        
        :else (recur valid-neighbors
                     new-visited
                     (inc counter))))))

(defn cost-heuristic [pos1 pos2]
  (reduce + (map - pos2 pos1)))

(defn reconstruct-path [came-from current]
  (when current
    (cons current (reconstruct-path came-from (came-from current)))))

;; Part 1
(let [start [0 0]
      goal [MAX-POS MAX-POS]
      h cost-heuristic
      corrupted (corrupted-space bytes fallen-bytes)
      dget #(get %1 %2 10000000000000)]
  (loop [came-from {}
         ;; gScore[n] is the currently known cost of the cheapest path from start to n.
         g-score {start 0}
         ;; fScore[n] := gScore[n] + h(n), represents our current best guess as to how cheap a path could be from start to finish if it goes through n.
         f-score {start (h start goal)}
         open-set #{start}
         visited #{}
         counter 0]
    (let [sorted-open-set (sort-by #(dget f-score %) open-set)
          current (first sorted-open-set)
          open-set-minus-current (rest sorted-open-set)
          new-visited (conj visited current)
          valid-neighbors (filter (valid-next-step? corrupted visited) (get-neighboars current))
          tentative-score (inc (dget g-score current))
          [new-came-from new-g-score
           new-f-score new-open-set] (reduce
                                       (fn [[came-from g-score f-score open-set] neighbor]
                                         (if (< tentative-score (dget g-score neighbor))
                                           [(assoc came-from neighbor current)
                                            (assoc g-score neighbor tentative-score)
                                            (assoc f-score neighbor (+ tentative-score (h neighbor goal)))
                                            (conj open-set neighbor)]
                                           [came-from g-score f-score open-set]))
                                       [came-from g-score f-score open-set-minus-current]
                                       valid-neighbors)]
      (def came-from new-came-from)
      (def g-score new-g-score)
      (def f-score new-f-score)
      (def open-set new-open-set)
      (def visited new-visited)
      (def counter (inc counter))

      (cond
        (= 7000 counter) (println "counter" counter)

        (empty? open-set) (println "err" counter)

        (= current goal) (g-score current) ;;(reconstruct-path came-from current))
        
        :else (recur new-came-from new-g-score new-f-score new-open-set new-visited (inc counter))))))

;; Part 2
(defn x [fallen-bytes]
  (let [start [0 0]
        goal [MAX-POS MAX-POS]
        h cost-heuristic
        corrupted (corrupted-space bytes fallen-bytes)
        dget #(get %1 %2 10000000000000)]
    (loop [came-from {}
           ;; gScore[n] is the currently known cost of the cheapest path from start to n.
           g-score {start 0}
           ;; fScore[n] := gScore[n] + h(n), represents our current best guess as to how cheap a path could be from start to finish if it goes through n.
           f-score {start (h start goal)}
           open-set #{start}
           visited #{}
           counter 0]
      (let [sorted-open-set (sort-by #(dget f-score %) open-set)
            current (first sorted-open-set)
            open-set-minus-current (rest sorted-open-set)
            new-visited (conj visited current)
            valid-neighbors (filter (valid-next-step? corrupted visited) (get-neighboars current))
            tentative-score (inc (dget g-score current))
            [new-came-from new-g-score
             new-f-score new-open-set] (reduce
                                         (fn [[came-from g-score f-score open-set] neighbor]
                                           (if (< tentative-score (dget g-score neighbor))
                                             [(assoc came-from neighbor current)
                                              (assoc g-score neighbor tentative-score)
                                              (assoc f-score neighbor (+ tentative-score (h neighbor goal)))
                                              (conj open-set neighbor)]
                                             [came-from g-score f-score open-set]))
                                         [came-from g-score f-score open-set-minus-current]
                                         valid-neighbors)]
        (cond
          (= 10000 counter) (println "counter" counter)

          (empty? open-set) (println "err" counter)

          (= current goal) (g-score current) ;;(reconstruct-path came-from current))
          
          :else (recur new-came-from new-g-score new-f-score new-open-set new-visited (inc counter)))))))

;; Binary Search
(loop [good-bytes 3024
       bad-bytes 3027
       counter 0]
  (let [diff (- bad-bytes good-bytes)
        new-to-search (+ good-bytes (quot diff 2))
        found? (try (x new-to-search) (catch Exception _e false))]
    (println [good-bytes bad-bytes])
    (cond
      (> counter 1000) (println "counter" [good-bytes bad-bytes]) 
      (< diff 2) [good-bytes bad-bytes]
      found? (recur new-to-search bad-bytes (inc counter))
      :else (recur good-bytes new-to-search (inc counter)))))

(last (corrupted-space bytes 3027))

;; Printing board
(doseq [y (range (inc MAX-POS))
        x (range (inc MAX-POS))]
  (when (zero? x)
    (println))
  (cond
    (contains? (set (corrupted-space bytes fallen-bytes)) [x y]) 
    (print "#")

    (contains? (set (reconstruct-path came-from [1 0])) [x y])
    (print "0")

    :else
    (print ".")))
