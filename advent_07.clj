(require '[clojure.string :as string])

;; Part 1

(def equations
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn apply-ops [factors ops]
  (reduce (fn [acc [fact op]] (op acc fact))
          (first factors) (map #(into [%1 %2]) (rest factors) ops)))

(defn calc-possible-ops [n]
  (let [vars (repeatedly n gensym)]
    (eval `(for [~@(mapcat (fn [v] [v '[* + #(BigInteger. (str %1 %2))]]) vars)]
             [~@vars]))))

(defn true-equation? [[result & factors]]
  (let [;[result & factors] line
        op-count (dec (count factors))
        possible-ops (calc-possible-ops op-count)
        all-results (map (partial apply-ops factors) possible-ops)]
    (contains? (set all-results) result)))
    ;; (<= 0 (java.util.Collections/binarySearch all-results result compare))))

(->> equations
  (string/split-lines)
  (map #(re-seq #"\d+" %))
  (map (fn [l] (map #(BigInteger. %) l)))
  ;; last)
  (filter true-equation?)
  (map first)
  (reduce +))
