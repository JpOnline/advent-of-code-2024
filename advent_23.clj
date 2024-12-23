(def network "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(require '[clojure.string :as s])
(require '[clojure.math.combinatorics :as combo])

;; Part 1
(defn ->sets-of-3 [c-sets]
  (reduce
    (fn [sets-of-3 [c sets]]
      (into sets-of-3 (flatten
                        (map #(for [c3 (clojure.set/intersection
                                         (c-sets c)
                                         (c-sets %))]
                                #{c % c3})
                             sets))))
    #{}
    c-sets))

(->> network
  (re-seq #"([a-z]{2})-([a-z]{2})")
  (map (fn [[_ c1 c2]] [c1 c2]))
  (reduce (fn [c-sets [c1 c2]]
            (-> c-sets
              (update c1 #(if (nil? %) #{c2} (conj % c2)))
              (update c2 #(if (nil? %) #{c1} (conj % c1)))))
          {})
  (->sets-of-3)
  (filter #(some (fn [c] (s/starts-with? c "t")) %))
  (count))

;; Part 2
(def memory (atom {}))

(def expand-connections
  (memoize
    (fn [acc c-sets]
      (let [common (apply clojure.set/intersection (pmap c-sets acc))]
        (if (seq common)
          (pmap #(expand-connections (conj acc %) c-sets) common)
          (do
            (doseq [pair (combo/combinations acc 2)]
              (swap! memory assoc (set pair) acc)) ;; It probably speedup things towards the end.
            acc))))))

(def counter (atom 0))

(let [pairs (->> network
              (re-seq #"([a-z]{2})-([a-z]{2})")
              (map (fn [[_ c1 c2]] [c1 c2])))
      pair-set (map set pairs)
      c-sets (reduce (fn [c-sets [c1 c2]]
                        (-> c-sets
                          (update c1 #(if (nil? %) #{c2} (conj % c2)))
                          (update c2 #(if (nil? %) #{c1} (conj % c1)))))
                     {}
                     pairs)
      total-pairs (count pairs)]
  (->> pair-set
    (pmap #(do
             (println (str @counter"/"total-pairs)) ;; To see the progress of the execution.
             (swap! counter inc)
             (if-let [res (@memory %)]
               res
               (expand-connections % c-sets))))
    (flatten)
    (apply max-key count)
    (sort)
    (s/join ",")))
