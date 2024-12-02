(def l1 [3 4 2 1 3 3])
   
(def l2 [4 3 5 3 9 3])

;; Part 1
(reduce + (map abs (map - (sort l1) (sort l2))))

;; Part 2
(reduce + (map * l1 (map #(reduce (fn [s a] (if (= a %) (inc s) s)) 0 l2) l1)))
