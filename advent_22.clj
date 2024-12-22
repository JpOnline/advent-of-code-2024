(defn next-secret [secret]
  (as-> secret $
    (* $ 64)
    (bit-xor $ secret)
    (mod $ 16777216)
    (as-> $ $$
      (quot $$ 32)
      (bit-xor $$ $)
      (mod $$ 16777216))
    (as-> $ $$$
      (* $$$ 2048)
      (bit-xor $$$ $)
      (mod $$$ 16777216))))

;; part 1
(reduce + (map #(nth (iterate next-secret %) 2000) secrets))

;; Part 2
(def last-digit #(mod % 10))
(def part-4-seq #(partition 4 1 %))
(defn seq-of-4-and-value [[[_ s1] [_ s2] [_ s3] [v s4]]]
  {[s1 s2 s3 s4] v})

(->> secrets
  (map #(->> %
          (iterate next-secret)
          (take 2001)
          (map last-digit)))
  (pmap #(partition 2 1 %))
  (pmap #(map (fn [[i f]] [f (- f i)]) %))
  (pmap part-4-seq)
  (pmap #(map seq-of-4-and-value %))
  (map #(apply merge-with (fn [first-el _second-el] first-el) %))
  (apply merge-with +)
  (apply max-key val))
     
;; Let's take a simple example to understand what's happening with buyers
;; with secrets 1, 2 and 2024.
(->> [1 2 2024]

  ;; This is generating a sequence of 6 banana prices, for the solution we
  ;; need a sequence of 2001.
  ;; ((1 3 3 9 5 9) (2 6 2 4 0 9) (4 9 5 3 4 3))
  (map #(->> %
          (iterate next-secret)
          (take 6)
          (map last-digit)))

  ;; (((1 3) (3 3) (3 9) (9 5) (5 9))
  ;;  ((2 6) (6 2) (2 4) (4 0) (0 9))
  ;;  ((4 9) (9 5) (5 3) (3 4) (4 3)))
  (pmap #(partition 2 1 %))

  ;; These are lists of the value with the difference between last 2 prices.
  ;; (([3 2] [3 0] [9 6] [5 -4] [9 4])
  ;;  ([6 4] [2 -4] [4 2] [0 -4] [9 9])
  ;;  ([9 5] [5 -4] [3 -2] [4 1] [3 -1]))
  (pmap #(map (fn [[i f]] [f (- f i)]) %))

  ;; For each buyer, we can see all the sequences of 4 prices and how many
  ;; bananas we would get for that sequence. It's in a map where the key is
  ;; the sequence and the value is the price.
  ;; (({[2 0 6 -4] 5} {[0 6 -4 4] 9})
  ;;  ({[4 -4 2 -4] 0} {[-4 2 -4 9] 9})
  ;;  ({[5 -4 -2 1] 4} {[-4 -2 1 -1] 3}))
  (pmap part-4-seq)
  (pmap #(map seq-of-4-and-value %))

  ;; We want to merge the maps of each buyer, but default behavior of merge
  ;; is to keep last occurrence. With this we can keep the first occurrence.
  (map #(apply merge-with (fn [first-el _second-el] first-el) %))

  ;; This will merge all buyers maps, and for repeated keys (i.e. sequences of
  ;; prices differences) it'll sum the value (i.e. the amount of bananas we get).
  (apply merge-with +)

  ;; Now we just need to know what's the key with higher value.
  (apply max-key val))
