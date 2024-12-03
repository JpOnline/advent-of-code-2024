(def instruction "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

;; Part 1

(->> instruction
  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
  (map (fn [[s f1 f2]] [(Integer/parseInt f1) (Integer/parseInt f2)]))
  (map #(apply * %))
  (reduce +))

;; Part 2

(def instruction "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(->> instruction
  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|don't\(\)|do\(\)")
  (reduce (fn [[status acc] [s _ _ :as ins]]
            (cond
              (= s "do()") [:enabled acc]
              (= s "don't()") [:disabled acc]
              (= status :enabled) [status (conj acc ins)]
              (= status :disabled) [status acc]))
          [:enabled []])
  (second)
  (map (fn [[s f1 f2]] [(Integer/parseInt f1) (Integer/parseInt f2)]))
  (map #(apply * %))
  (reduce +))
