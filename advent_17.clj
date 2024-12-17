(require '[clojure.math :as math])


(def state {:A 598851513
            :B 0
            :C 0
            :program [2,4,1,4,7,5,4,1,1,4,5,5,0,3,3,0]})


(def output "")

(defn combo-operand
  [operand state]
  (cond
    (<= operand 3) operand
    (= operand 4) (:A state)
    (= operand 5) (:B state)
    (= operand 6) (:C state)))

(defmulti compute-step (fn [_state instruction _] instruction))

(def debug-1 false)

(defmethod compute-step 0 ;; adv
  [{:keys [A] :as state} _instruction operand]
  (when debug-1
    (println "adv")
    (println "A" A))
  ;; (println "operand" operand)
  ;; (println "combo" (combo-operand operand state))
  ;; (println "pow" (math/pow 2 (combo-operand operand state)))
  ;; (println "div" (math/floor-div A (math/pow 2 (combo-operand operand state))))
  (assoc state :A (math/floor-div A (math/pow 2 (combo-operand operand state)))))

(defmethod compute-step 1 ;; bxl
  [state _instruction operand]
  (when debug-1
    (println "bxl")
    (println "A" (:A state)))
  (update state :B bit-xor operand))

(defmethod compute-step 2 ;; bst
  [state _instruction operand]
  (when debug-1
    (println "bst")
    (println "A" (:A state)))
  (assoc state :B (mod (combo-operand operand state) 8)))

(defmethod compute-step 3 ;; jnz
  [{:keys [A] :as state} _instruction operand]
  (when debug-1
    (println)
    (println "jnz")
    (println "A" (:A state)))
  (if (zero? A)
    state
    (assoc state :instruction-pointer (- operand 2))))

(defmethod compute-step 4 ;; bxc
  [{:keys [C] :as state} _instruction _operand]
  (when debug-1
    (println "bxc")
    (println "A" (:A state)))
  (update state :B bit-xor C))

(defmethod compute-step 5 ;; out
  [state _instruction operand]
  (when debug-1
    (println "out")
    (println "A" (:A state))
    (print (str (mod (combo-operand operand state) 8)",")))
  (def output (str output (mod (combo-operand operand state) 8) ","))
  state)

(defmethod compute-step 6 ;; bdv
  [{:keys [A] :as state} _instruction operand]
  (when debug-1
    (println "bdv")
    (println "A" (:A state)))
  (assoc state :B (math/floor-div A (math/pow 2 (combo-operand operand state)))))

(defmethod compute-step 7 ;; cdv
  [{:keys [A] :as state} _instruction operand]
  (when debug-1
    (println "cdv")
    (println "operand" operand)
    (println "combo" (combo-operand operand state))
    (println "pow" (math/pow 2 (combo-operand operand state)))
    (println "div" (math/floor-div A (math/pow 2 (combo-operand operand state))))
    (println "A" (:A state))
    (println state))
  (assoc state :C (math/floor-div A (math/pow 2 (combo-operand operand state)))))

(defmethod compute-step :end
  [state _ _]
  (when debug-1
    (println "end")
    (println "A" (:A state)))
  state)

(loop [state (assoc state :instruction-pointer 0)]
  (def state* state)
  ;; (when (= 8 (:instruction-pointer state)) (println state))
  (let [instruction-pointer (:instruction-pointer state)
        ;; _ (print "+"instruction-pointer)
        opcode (get (:program state) instruction-pointer :end)
        ;; _ (print " |"opcode",")
        operand (get (:program state) (inc instruction-pointer) :none)
        new-state (try (compute-step state opcode operand)
                       (catch Exception e
                         (println state)
                         (ex-info "Err" {:state state} e)))]
                         
    (if (or #_(>= instruction-pointer 14)
            (= :end opcode))
      (do
        (println output)
        (def output "")
        new-state)
      (recur (update new-state :instruction-pointer (comp inc inc))))))

;; Part 2

(defn test-sequence [initial-A]
  (loop [state (assoc state :instruction-pointer 0 :A initial-A)]
    (def state* state)
    ;; (when (= 8 (:instruction-pointer state)) (println state))
    (let [instruction-pointer (:instruction-pointer state)
          ;; _ (print "+"instruction-pointer)
          opcode (get (:program state) instruction-pointer :end)
          ;; _ (print " |"opcode",")
          operand (get (:program state) (inc instruction-pointer) :none)
          new-state (try (compute-step state opcode operand)
                         (catch Exception e
                           (println state)
                           (ex-info "Err" {:state state} e)))
          new-output output]

      (if (or #_(<= (:instruction-pointer new-state) -2)
              (= :end opcode))
        (do
          ;; (println output)
          (def output "")
          {:output new-output
           :state new-state})
        (recur (update new-state :instruction-pointer (comp inc inc)))))))

(let [expected-output "2,4,1,4,7,5,4,1,1,4,5,5,0,3,3,0,"
      char->int #(parse-long (str (first %)))]
  (loop [res 1
         counter 0
         next-value-to-search-pointer 30] 
    (let [crop-expected (subs expected-output next-value-to-search-pointer)
          next-value-to-search (char->int crop-expected)]
      (cond
        (> counter 1000000)
        (do (println "nop" counter crop-expected) res)

        (= expected-output (:output (test-sequence res)))
        res

        (= crop-expected (:output (test-sequence res)))
        (do (println counter)
            (recur (calculate-new-value (* 8 res) next-value-to-search)
                   0
                   (- next-value-to-search-pointer 2)))

        :else (recur (calculate-new-value (inc res) next-value-to-search-pointer)
                     (inc counter)
                     next-value-to-search-pointer)))))

(defn calculate-new-value [old-initial next-sequence-value]
  (loop [initial-A old-initial]
    (let [loop-result (loop [state (assoc state :instruction-pointer 0 :A initial-A)]
                        (def state* state)
                        ;; (when (= 8 (:instruction-pointer state)) (println state))
                        (let [instruction-pointer (:instruction-pointer state)
                              ;; _ (print "+"instruction-pointer)
                              opcode (get (:program state) instruction-pointer :end)
                              ;; _ (print " |"opcode",")
                              operand (get (:program state) (inc instruction-pointer) :none)
                              new-state (try (compute-step state opcode operand)
                                             (catch Exception e
                                               (println state)
                                               (ex-info "Err" {:state state} e)))
                              new-output output]

                          (if (or (<= (:instruction-pointer new-state) -2)
                                  (= :end opcode))
                            (do
                              ;; (println output)
                              (def output "")
                              {:output new-output
                               :state new-state})
                            (recur (update new-state :instruction-pointer (comp inc inc))))))]
      ;; (println)
      ;; (println "init a")
      ;; (println initial-A)
      ;; (println (:output loop-result))
      (cond 
        (> initial-A (* old-initial 30))
        {:initial-A initial-A
         :output (:output loop-result)
         :loop-result loop-result}

        (and (= (str next-sequence-value",") (:output loop-result)))
             ;; (= 0 (:B (:state loop-result)))
             ;; (= 0 (:C (:state loop-result))))
        initial-A

        :else
        (recur (inc initial-A))))))
