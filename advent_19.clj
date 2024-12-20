(def available* "r, wr, b, g, bwu, rb, gb, br")
(def desired* "brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(require '[clojure.string :as s])

(def available* "b, bwu, wurw")
;; I need a test case where it finds a big towel that matches, but the answer is with a small towel.
(def towel "bwurw")
;; I need a test case where it finds a small towel that matches, but the answer is with a big towel.
(def towel "bwubwu")

(def available* "b, bwu, bw, urw")
(def towel "bwurw")

;; Part 1
(def possible-towel?
  (memoize
    (fn [available towel]
      ;; (println towel)
      ;; (def counter-2 (inc counter-2))
      ;; (println (str counter-2" "))
      ;; (if (> counter-2 1500)
      ;;   (do (println "counter") false)
      (let [towel-size (count towel)
            all-sizes (for [n (reverse (range towel-size))]
                        (subs towel 0 (inc n)))]
        (loop [possibilities (filter (set available) all-sizes)
               counter 0]
          ;; (println towel)
          ;; (println possibilities)
          ;; (println)
          (let [[curr-poss & r-poss] possibilities
                possibility-size (count curr-poss)]
            (cond
              (> counter 5000) (print "err")

              (= possibility-size towel-size) true

              (empty? possibilities) false

              (not (possible-towel? available (subs towel possibility-size towel-size)))
              (recur r-poss (inc counter))

              :else true)))))))

(let [available (s/split available* #", ")
      desired (s/split-lines desired*)]
  (count (filter #(possible-towel? available %) desired)))

;; Part 2    
(def towel-possibilities-nr
  (memoize
    (fn
      ;; Overloading, same function with 2 args
      ([available towel]
       (let [towel-size (count towel)
             all-sizes (for [n (reverse (range towel-size))]
                         (subs towel 0 (inc n)))
             possibilities (filter (set available) all-sizes)]
         (towel-possibilities-nr available towel possibilities 0)))
      ;; Overloading, same function with 4 args
      ( [available towel [curr-poss & r-poss :as possibilities] number-of-possibilities]
      ;; (println towel)
      ;; (def counter-2 (inc counter-2))
      ;; (println (str counter-2" "))
      ;; (if (> counter-2 1500)
      ;;   (do (println "counter") false)
        (let [towel-size (count towel)
              possibility-size (count curr-poss)

              sub-towel (subs towel possibility-size towel-size)
              subtowel-size (count sub-towel)
              all-sizes-subtowel (for [n (reverse (range subtowel-size))]
                                   (subs sub-towel 0 (inc n)))
              new-possibilities (filter (set available) all-sizes-subtowel)]
            ;; (println towel)
            ;; (println possibilities)
            ;; (println)
          (cond
            (= possibility-size towel-size)
            (+ (inc number-of-possibilities)
               (towel-possibilities-nr available towel r-poss number-of-possibilities))

            (empty? possibilities) 0

            :else (+ (towel-possibilities-nr available sub-towel new-possibilities number-of-possibilities)
                     (towel-possibilities-nr available towel r-poss number-of-possibilities))))))))
    
(let [available (s/split available* #", ")
      desired (s/split-lines desired*)]
  (reduce + (map #(towel-possibilities-nr available (s/trim %)) desired)))
