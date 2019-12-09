(ns clj-adventofcode2019.puzzle7
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn- extract-from-program-code
  [program-code running-idx cnt]
  (subvec @program-code (inc @running-idx) cnt))

(defn- load-code
  []
  (into [] (map #(-> %
                     (clojure.string/trim)
                     (Integer/parseInt))
                (clojure.string/split
                 ;(slurp "resources/puzzle_pt2_test2.txt")
                 (slurp "resources/puzzle7.txt")
                 #","))))

(defn- operation
  [operation-fn first-value second-value result-idx program-code]
  (assoc
    @program-code
    result-idx
    (operation-fn first-value second-value)))

(defn- addition
  ([config]
   (fn [first-value second-value result-idx]
     (addition config first-value second-value result-idx (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value result-idx program-code running-idx argcount]
   (assoc config :program-code (operation + first-value second-value result-idx program-code) :running-idx (+ (inc argcount) @running-idx))))

(defn- multiplication
  ([config]
   (fn [first-value second-value result-idx]
     (multiplication config first-value second-value result-idx (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value result-idx program-code running-idx argcount]
   (assoc config :program-code (operation * first-value second-value result-idx program-code) :running-idx (+ (inc argcount) @running-idx))))
;;    (operation * first-value second-value result-idx program-code)
;;    (swap! running-idx #(+ % (inc argcount)))))

(defn- put-in-pos
  ([config]
   (fn [first-value]
     (put-in-pos config first-value (:program-code config) (:running-idx config) (:argcount config) (:input config))))
  ([config first-value program-code running-idx argcount input]
                                        ;   (let [input (Integer/valueOf (read-line))]
   (assoc
    config
    :program-code (assoc @program-code first-value input)
    :running-idx (+ (inc argcount) @running-idx))))
;;    (swap! program-code #(assoc % first-value input))
;;    (swap! running-idx #(+ % (inc argcount)))))

(defn- print-pos
  ([config]
   (fn [first-value]
     (print-pos config first-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value program-code running-idx argcount]
   (assoc
    config
    :program-code @program-code
    :running-idx (+ (inc argcount) @running-idx)
    :output first-value)))

(defn- jump-if-true
  ([config]
   (fn [first-value second-value]
     (jump-if-true config first-value second-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value program-code running-idx argcount]
   (if (not= 0 first-value)
     (assoc config :program-code @program-code :running-idx second-value)
     (assoc config :program-code @program-code :running-idx (+ (inc argcount) @running-idx)))))

(defn- jump-if-false
  ([config]
   (fn [first-value second-value]
     (jump-if-false config first-value second-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value program-code running-idx argcount]
   (if (not= 0 first-value)
     (assoc config :program-code @program-code :running-idx second-value)
     (assoc config :program-code @program-code :running-idx (+ (inc argcount) @running-idx)))))

(defn- less-than
  ([config]
   (fn [first-value second-value third-value]
     (less-than config first-value second-value third-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value third-value program-code running-idx argcount]
   (print "less than")
   (if (< first-value second-value)
     (assoc config :program-code (assoc @program-code third-value 1) :running-idx (+ (inc argcount) @running-idx))
     (assoc config :program-code (assoc @program-code third-value 0) :running-idx (+ (inc argcount) @running-idx)))))

(defn- equals
  ([config]
   (fn [first-value second-value third-value]
     (equals config first-value second-value third-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value third-value program-code running-idx argcount]
   (println "equals")
   (if (= first-value second-value)
     (assoc config :program-code (assoc @program-code third-value 1) :running-idx (+ (inc argcount) @running-idx))
     (assoc config :program-code (assoc @program-code third-value 0) :running-idx (+ (inc argcount) @running-idx)))))

(defn- extract-values-from-program-code
  [program-code running-idx cnt mask]
  (map
   (fn [mask-n code-n]
     (if mask-n
       (nth @program-code code-n)
       code-n))
   mask
   (subvec @program-code (+ 1 @running-idx) (+ (inc cnt) @running-idx))))

(def configs-per-operation
  {1 {:argcount 3
      :fn addition
      :last-mask false}
   2 {:argcount 3
      :fn multiplication
      :last-mask false}
   3 {:argcount 1
      :fn put-in-pos
      :last-mask false}
   4 {:argcount 1
      :fn print-pos}
   5 {:argcount 2
      :fn jump-if-true}
   6 {:argcount 2
      :fn jump-if-false}
   7 {:argcount 3
      :fn less-than
      :last-mask false}
   8 {:argcount 3
      :fn equals
      :last-mask false}})

(defn- extract-operation-and-mask
  [opcode]
  (let [operation (mod opcode 100)
        modes (int (/ opcode 100))]
    (if (contains? (get configs-per-operation operation) :last-mask)
      {:operation operation
       :mask (reverse
              (cons (:last-mask (get configs-per-operation operation))
                    (rest (map
                           #(= % \0)
                           (format (str "%0" (:argcount (get configs-per-operation operation)) "d") modes)))))}
      {:operation operation
       :mask (reverse
              (map #(= % \0)
                   (format (str "%0" (:argcount (get configs-per-operation operation)) "d") modes)))})))

(defn- calculate-output
  ([running-idx inputs]
   (calculate-output (atom (load-code)) running-idx inputs))
  ([state]
   (let [suspend (atom false)
         last-output (atom nil)]
     (while (and (not @suspend) (not= 99 (nth @(:program-code state) @(:running-idx state))))
       
       (let [opcode (nth @(:program-code state) @(:running-idx state))
             operation-and-mask (extract-operation-and-mask opcode)
             operation (:operation operation-and-mask)
             operation-configs (get configs-per-operation operation)
             current-input (if (not @(:initialised state))
                             (first @(:inputs state))
                             (nth @(:inputs state) 1 (first @(:inputs state))))
             values-for-operation (extract-values-from-program-code
                                   (:program-code state)
                                   (:running-idx state)
                                   (:argcount operation-configs)
                                   (:mask operation-and-mask))
             old-configs {:program-code (:program-code state)
                          :running-idx (:running-idx state)
                          :argcount (:argcount operation-configs)
                          :input current-input}]
         (cond
           (= 3 operation) (do
                                (if (not @(:initialised state))
                                  (swap! (:initialised state) (fn [_] true))
                                  (swap! (:inputs state) #(rest %)))
                                (let [new-configs (apply ((:fn operation-configs) old-configs) values-for-operation)]
                                  (swap! (:program-code state) (fn [_] (get new-configs :program-code)))
                                  (swap! (:running-idx state) (fn [_] (get new-configs :running-idx)))
                                  (swap! (:inputs state) #(into [] (rest %)))))
           (= 4 operation) (do
                             (let [new-configs (apply ((:fn operation-configs) old-configs) values-for-operation)]
                               (swap! (:running-idx state) (fn [_] (get new-configs :running-idx)))
                               (swap! last-output (fn [_] (get new-configs :output)))
                               (swap! suspend (fn [_] true))))
              :else (let [new-configs (apply ((:fn operation-configs) old-configs) values-for-operation)]
                      (swap! (:program-code state) (fn [_] (get new-configs :program-code)))
                      (swap! (:running-idx state) (fn [_] (get new-configs :running-idx)))))
         {:suspend @suspend :program-code @(:program-code state) :running-idx @(:running-idx state)})
       )
;;       (let [opcode (nth @(:program-code state) @(:running-idx state))
;;             operation-and-mask (extract-operation-and-mask opcode)
;;             operation (:operation operation-and-mask)
;;             operation-configs (get configs-per-operation operation)
;;             current-input (if (not @(:initialised state))
;;                             (first @(:inputs state))
;;                             (nth @(:inputs state) 1 (first @(:inputs state))))
;;             values-for-operation (extract-values-from-program-code
;;                                     (:program-code state)
;;                                     (:running-idx state)
;;                                     (:argcount operation-configs)
;;                                     (:mask operation-and-mask))
;;             old-configs {:program-code (:program-code state)
;;                          :running-idx (:running-idx state)
;;                          :argcount (:argcount operation-configs)
;;                          :input current-input}]
;;         (cond
;;           (= 3 operation) (do
;;                             (if (not @(:initialised state))
;;                               (swap! (:initialised state) (fn [_] true))
;;                               (swap! (:inputs state) #(rest %)))
;;                             (let [new-configs (apply ((:fn operation-configs) old-configs) values-for-operation)]
;;                               (swap! (:program-code state) (fn [_] (get new-configs :program-code)))
;;                               (swap! (:running-idx state) (fn [_] (get new-configs :running-idx)))
;;                               (swap! (:inputs state) #(into [] (rest %)))))
;;           (= 4 operation) (do
;;                             (let [new-configs (apply ((:fn operation-configs) old-configs) values-for-operation)]
;;                               (swap! (:running-idx state) (fn [_] (get new-configs :running-idx)))
;;                               (swap! last-output (fn [_] (get new-configs :output)))
;;                               (swap! suspend (fn [_] true))))
;;           :else (let [new-configs (apply ((:fn operation-configs) old-configs) values-for-operation)]
;;                   (swap! (:program-code state) (fn [_] (get new-configs :program-code)))
;;                   (swap! (:running-idx state) (fn [_] (get new-configs :running-idx)))))))
     (when (= 99 (nth @(:program-code state) @(:running-idx state)))
       (swap! (:finished state) (fn [_] true)))
     @last-output)))

(defn run-pt1
  []
  (let [program-code (atom (load-code))
        running-idx (atom 0)]
    (apply max (map
                #(reduce
                  (fn [last-output cur-amp]
                    (let [calculated-output (calculate-output program-code running-idx (atom [cur-amp last-output]) (atom false))]
                      calculated-output))
                  0
                  %)
                (combo/permutations [0 1 2 3 4])))))

(defn init-state
  []
  (identity {0 {:program-code (atom (load-code))
                :running-idx (atom 0)
                :initialised (atom false)
                :finished (atom false)
                :inputs (atom [])
                :last-output (atom 0)}
             1 {:program-code (atom (load-code))
                :running-idx (atom 0)
                :initialised (atom false)
                :finished (atom false)
                :inputs (atom [])
                :last-output (atom 0)}
             2 {:program-code (atom (load-code))
                :running-idx (atom 0)
                :initialised (atom false)
                :finished (atom false)
                :inputs (atom [])
                :last-output (atom 0)}
             3 {:program-code (atom (load-code))
                :running-idx (atom 0)
                :initialised (atom false)
                :finished (atom false)
                :inputs (atom [])
                :last-output (atom 0)}
             4 {:program-code (atom (load-code))
                :running-idx (atom 0)
                :initialised (atom false)
                :finished (atom false)
                :inputs (atom [])
                :last-output (atom 0)}}))

(defn not-finished
  [state current-permutation]
  (fn [_] (not @(:finished (get @state (mod (last @current-permutation) 5))))))

(defn run-pt2
  []
  (let [state (atom (init-state))
        current-permutation (atom nil)]
    (apply
     max
     (map
      #(do
         (swap! current-permutation (fn [_] %))
         (let [output-outer
               (reduce
                (fn [last-output cur-amp-setting]
                  (let [cur-computer (mod cur-amp-setting 5)
                        cur-settings (get @state cur-computer)
                        new-inputs (swap! (:inputs cur-settings) (fn [_]
                                                                   (identity [cur-amp-setting (first last-output)])))
                        output (calculate-output cur-settings)]
                    (into [] (cons output last-output))))
                [0]
                (take-while (not-finished state current-permutation) (cycle %)))]
           (swap! state (fn [_] (init-state)))
           (first (remove nil? output-outer))))
      (combo/permutations [5 6 7 8 9])))))

