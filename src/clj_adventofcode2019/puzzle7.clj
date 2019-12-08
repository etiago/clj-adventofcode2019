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
  (swap!
   program-code
   #(assoc
     %
     result-idx
     (operation-fn first-value second-value))))

(defn- addition
  ([config]
   (fn [first-value second-value result-idx]
     (addition first-value second-value result-idx (:program-code config) (:running-idx config) (:argcount config))))
  ([first-value second-value result-idx program-code running-idx argcount]
   (operation + first-value second-value result-idx program-code)
   (swap! running-idx #(+ % (inc argcount)))))

(defn- multiplication
  ([config]
   (fn [first-value second-value result-idx]
     (multiplication first-value second-value result-idx (:program-code config) (:running-idx config) (:argcount config))))
  ([first-value second-value result-idx program-code running-idx argcount]
   (operation * first-value second-value result-idx program-code)
   (swap! running-idx #(+ % (inc argcount)))))

(defn- put-in-pos
  ([config]
   (fn [first-value]
     (put-in-pos first-value (:program-code config) (:running-idx config) (:argcount config) (:input config))))
  ([first-value program-code running-idx argcount input]
                                        ;   (let [input (Integer/valueOf (read-line))]
   (swap! program-code #(assoc % first-value input))
   (swap! running-idx #(+ % (inc argcount)))))

(defn- print-pos
  ([config]
   (fn [first-value]
     (print-pos first-value (:program-code config) (:running-idx config) (:argcount config))))
  ([first-value program-code running-idx argcount]
   (swap! running-idx #(+ % (inc argcount)))
   first-value))

(defn- jump-if-true
  ([config]
   (fn [first-value second-value]
     (jump-if-true first-value second-value (:program-code config) (:running-idx config) (:argcount config))))
  ([first-value second-value program-code running-idx argcount]
   (if (not= 0 first-value)
     (swap! running-idx (fn [_] (identity second-value)))
     (swap! running-idx #(+ % (inc argcount))))))

(defn- jump-if-false
  ([config]
   (fn [first-value second-value]
     (jump-if-false first-value second-value (:program-code config) (:running-idx config) (:argcount config))))
  ([first-value second-value program-code running-idx argcount]
   (if (= 0 first-value)
     (swap! running-idx (fn [_] (identity second-value)))
     (swap! running-idx #(+ % (inc argcount))))))

(defn- less-than
  ([config]
   (fn [first-value second-value third-value]
     (less-than first-value second-value third-value (:program-code config) (:running-idx config) (:argcount config))))
  ([first-value second-value third-value program-code running-idx argcount]
   (if (< first-value second-value)
     (swap! program-code #(assoc % third-value 1))
     (swap! program-code #(assoc % third-value 0)))
   (swap! running-idx #(+ % (inc argcount)))))

(defn- equals
  ([config]
   (fn [first-value second-value third-value]
     (equals first-value second-value third-value (:program-code config) (:running-idx config) (:argcount config))))
  ([first-value second-value third-value program-code running-idx argcount]
   (if (= first-value second-value)
     (swap! program-code #(assoc % third-value 1))
     (swap! program-code #(assoc % third-value 0)))
   (swap! running-idx #(+ % (inc argcount)))))

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
  ([program-code running-idx inputs initialised finished]
   (let [suspend (atom false)
         last-output (atom nil)]
     (while (and (not @suspend) (not= 99 (nth @program-code @running-idx)))
       (let [opcode (nth @program-code @running-idx)
             operation-and-mask (extract-operation-and-mask opcode)
             operation (:operation operation-and-mask)
             mask (:mask operation-and-mask)
             operation-configs (get configs-per-operation operation)
             current-input (if (not @initialised)
                             (first @inputs)
                             (nth @inputs 1 (first @inputs)))]
         (cond
           (= 3 operation) (do
                             (if (not @initialised)
                               (swap! initialised (fn [_] true))
                               (swap! inputs #(rest %)))
                             (apply
                              ((:fn operation-configs)
                               {:program-code program-code
                                :running-idx running-idx
                                :argcount (:argcount operation-configs)
                                :input current-input})
                              (extract-values-from-program-code
                               program-code
                               running-idx
                               (:argcount operation-configs)
                               mask))
                             (swap! inputs #(into [] (rest %))))
           (= 4 operation) (do
                             (let [output
                                   (apply
                                    ((:fn operation-configs)
                                     {:program-code program-code
                                      :running-idx running-idx
                                      :argcount (:argcount operation-configs)})
                                    (extract-values-from-program-code
                                     program-code
                                     running-idx
                                     (:argcount operation-configs)
                                     mask))]
                               (swap! last-output (fn [_] (identity output)))
                               (swap! suspend (fn [_] true))))
           :else (apply
                  ((:fn operation-configs)
                   {:program-code program-code
                    :running-idx running-idx
                    :argcount (:argcount operation-configs)})
                  (extract-values-from-program-code
                   program-code
                   running-idx
                   (:argcount operation-configs)
                   mask)))))
     (when (= 99 (nth @program-code @running-idx))
       (swap! finished (fn [_] true)))
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
                         output (calculate-output
                                 (:program-code cur-settings)
                                 (:running-idx cur-settings)
                                 (:inputs cur-settings)
                                 (:initialised cur-settings)
                                 (:finished cur-settings))]
                     (into [] (cons output last-output))))
                 [0]
                 (take-while (not-finished state current-permutation) (cycle %)))]
            (swap! state (fn [_] (init-state)))
            (first (remove nil? output-outer))))
       (combo/permutations [5 6 7 8 9])))))

