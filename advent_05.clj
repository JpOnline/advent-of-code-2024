(require '[clojure.string :as string])

;; Part 1

(def rules*
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13")

(def order*
"75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(def rules
  (->> rules*
    (string/split-lines)
    (map #(string/split % #"\|"))
    (mapv (fn [[a b]] [(Integer/parseInt a) (Integer/parseInt b)]))))

(def order
  (->> order*
    (string/split-lines)
    (map #(string/split % #"\,"))
    (mapv (fn [nrs] (mapv #(Integer/parseInt %) nrs)))))

(defn break-rule? [update]
  (->> update
    (reduce (fn [[c acc] n]
              [(inc c) (conj acc (mapv #(into [% n]) (drop c update)))])
            [1 []])
    (second)
    (apply concat)
    (set)
    (clojure.set/intersection (set rules))
    (seq)))

(->> order
  (remove break-rule?) 
  (map #(get % (quot (count %) 2)))
  (reduce +))

;; Part 2

(defn broken-rules [update]
  (->> update
    (reduce (fn [[c acc] n]
              [(inc c) (conj acc (mapv #(into [% n]) (drop c update)))])
            [1 []])
    (second)
    (apply concat)
    (set)
    (clojure.set/intersection (set rules))))

(defn fix-update [update]
  (let [[a b] (first (broken-rules update))
        new-update (reduce
                     (fn [acc n]
                       (cond
                         (= n a) (conj acc b)
                         (= n b) (conj acc a)
                         :else (conj acc n)))
                     [] update)]
    (if (empty? (broken-rules new-update))
      new-update
      (recur new-update))))

(->> order
  (filter break-rule?)
  (map fix-update)
  (map #(get % (quot (count %) 2)))
  (reduce +))
