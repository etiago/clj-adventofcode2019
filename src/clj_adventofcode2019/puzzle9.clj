(ns clj-adventofcode2019.puzzle9
  (:gen-class)
  (:require [clojure.set :as set]))

(defn- extract-from-program-code
  [program-code running-idx cnt]
  (subvec program-code (inc running-idx) cnt))

(defn load-code
  []
  (apply merge (map-indexed (fn [key value]
         {key value})
         (map #(-> %
                   (clojure.string/trim)
                                        (biginteger)
                   ;(Integer/parseInt)
                   )
                     ;(Integer/parseInt))
                (clojure.string/split
                 ;(slurp "resources/puzzle_pt2_test2.txt")
                 (slurp "resources/puzzle9.txt")
                 #",")) )))

(defn- operation
  [operation-fn first-value second-value result-idx program-code]
  (merge program-code {result-idx (operation-fn first-value second-value)}))

(defn- addition
  ([config]
   (fn [first-value second-value result-idx]
     (addition config first-value second-value result-idx (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value result-idx program-code running-idx argcount]
   (assoc config
          :program-code (operation + first-value second-value result-idx program-code)
          :running-idx (+ running-idx (inc argcount)))))

(defn- multiplication
  ([config]
   (fn [first-value second-value result-idx]
     (multiplication config first-value second-value result-idx (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value result-idx program-code running-idx argcount]
   (assoc config
          :program-code (operation * first-value second-value result-idx program-code)
          :running-idx (+ running-idx (inc argcount)))))

(defn- put-in-pos
  ([config]
   (fn [first-value]
     (put-in-pos config first-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value program-code running-idx argcount]
   (let [input (if (get config :read-from-stdin true) (Integer/valueOf (read-line)) (get config :input))]
     (assoc config
            :program-code (merge program-code {first-value input})
            :running-idx (+ running-idx (inc argcount))))))

(defn- print-pos
  ([config]
   (fn [first-value]
     (print-pos config first-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value program-code running-idx argcount]
   ;(println first-value)
   (assoc config :running-idx (+ running-idx (inc argcount)) :output first-value :last-operation 4)))

(defn- jump-if-true
  ([config]
   (fn [first-value second-value]
     (jump-if-true config first-value second-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value program-code running-idx argcount]
   (if (not= 0 first-value)
     (assoc config :running-idx second-value)
     (assoc config :running-idx (+ running-idx (inc argcount))))))

(defn- jump-if-false
  ([config]
   (fn [first-value second-value]
     (jump-if-false config first-value second-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value program-code running-idx argcount]
   (if (= 0 first-value)
     (assoc config :running-idx second-value)
     (assoc config :running-idx (+ running-idx (inc argcount))))))

(defn- less-than
  ([config]
   (fn [first-value second-value third-value]
     (less-than config first-value second-value third-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value third-value program-code running-idx argcount]
   (if (< first-value second-value)
     (assoc config :program-code (merge program-code {third-value 1}) :running-idx (+ running-idx (inc argcount)))
     (assoc config :program-code (merge program-code {third-value 0}) :running-idx (+ running-idx (inc argcount))))))

(defn- equals
  ([config]
   (fn [first-value second-value third-value]
     (equals config first-value second-value third-value (:program-code config) (:running-idx config) (:argcount config))))
  ([config first-value second-value third-value program-code running-idx argcount]
   (if (= first-value second-value)
     (assoc config :program-code (merge program-code {third-value 1}) :running-idx (+ running-idx (inc argcount)))
     (assoc config :program-code (merge program-code {third-value 0}) :running-idx (+ running-idx (inc argcount))))))

(defn- adjust-relative-base
  ([config]
   (fn [first-value]
     (adjust-relative-base config first-value (:program-code config) (:running-idx config) (:argcount config) (:input config) (:relative-start config))))
  ([config first-value program-code running-idx argcount input relative-start]
   (assoc config :relative-start (+ relative-start first-value) :running-idx (+ running-idx (inc argcount)))))

(defn- extract-values-from-program-code
  [program-code running-idx relative-base cnt mask last-pos-writing]
  (let [range-to-get (range (+ 1 running-idx) (+ (inc cnt) running-idx))
        keys-missing (clojure.set/difference (into #{} range-to-get) (into #{} (keys program-code)))
        dict-of-missing (map #(identity {% 0}) keys-missing)
        last-pos (last range-to-get)]
    ;(println (str "extracting values ... mask is " mask))
  (map
   (fn [mask-n code-n]
     (if (and last-pos-writing (= code-n last-pos))
       (cond
         (= mask-n \0) (get program-code code-n 0) 
         (= mask-n \1) (get program-code code-n 0)
         (= mask-n \2) (+ relative-base (get program-code code-n 0)))
       (cond
         (= mask-n \0) (get program-code (get program-code code-n) 0)
         (= mask-n \1) (get program-code code-n 0)
         (= mask-n \2) (get program-code (+ (get program-code code-n 0) relative-base) 0))
       )
     )
   mask
   (keys (select-keys (apply merge program-code dict-of-missing) range-to-get)))))



(def configs-per-operation
  {1 {:argcount 3
      :fn addition
      :last-mask \1
      :last-post-writing true
      ;:mask [true true false]
      }
   2 {:argcount 3
      :fn multiplication
      :last-mask \1
      :last-post-writing true
      ;:mask [true true false]
      }
   3 {:argcount 1
      :fn put-in-pos
      :last-mask \1
      :last-post-writing true
      ;:mask [false]
      }
   4 {:argcount 1
      :fn print-pos
      :last-mask \0
      ;:last-mask true
      ;:mask [true]
      }
   5 {:argcount 2
      :fn jump-if-true
      :last-mask \0
      ;:last-mask false
      }
   6 {:argcount 2
      :fn jump-if-false
      :last-mask \0
      ;:last-mask false
      }
   7 {:argcount 3
      :fn less-than
      :last-mask \1
      :last-post-writing true
      }
   8 {:argcount 3
      :fn equals
      :last-mask \1
      :last-post-writing true}
   9 {:argcount 1
      :fn adjust-relative-base
      :last-mask \0
      }
   99 {:argcount 1
       :last-mask \0}
      })

(defn- extract-operation-and-mask
  [opcode]
  (let [operation (mod opcode 100)
        modes (int (/ opcode 100))
        modes-str (String/valueOf modes)]
    (if (not= (count modes-str) (:argcount (get configs-per-operation operation))) ;(contains? (get configs-per-operation operation) :last-mask)
      {:operation operation
       :mask (reverse
              (cons (:last-mask (get configs-per-operation operation))
                     (rest (format (str "%0" (:argcount (get configs-per-operation operation)) "d") modes))))}
      {:operation operation
             :mask (reverse (format (str "%0" (:argcount (get configs-per-operation operation)) "d") modes))})))


(defn calculate-output-for-state
  ([]
   (calculate-output-for-state {:program-code (load-code)
                                :running-idx 0
                                :relative-start 0
                                :input 1
                                :read-from-stdin false
                                :last-operation -1}))
  ([state]
   (let [program-code (:program-code state)
         running-idx (:running-idx state)
         operation-and-mask (extract-operation-and-mask (get program-code running-idx))
         operation-configs (get configs-per-operation (:operation operation-and-mask))
         new-state {:program-code program-code
                    :running-idx running-idx
                    :argcount (:argcount operation-configs)
                    :relative-start (:relative-start state)
                    :input (:input state)
                    :read-from-stdin (:read-from-stdin state)
}
         values (extract-values-from-program-code
                                    program-code
                                    running-idx
                                    (:relative-start state)
                                    (:argcount operation-configs)
                                    (:mask operation-and-mask)
                                    (:last-post-writing operation-configs))]
     (cons
      state
     (lazy-seq

       (calculate-output-for-state (apply ((:fn operation-configs) new-state) values)))))))


  
(defn run-pt1
  ([]
   (run-pt1 {:program-code (load-code)
             :running-idx 0
             :relative-start 0
             :input 1
             :read-from-stdin false
             :last-operation -1}))
  ([state]
   (let [computer (calculate-output-for-state state)
         new-state (first (second (partition-by
                           #(not (contains? % :output));not= 4 (:last-operation %))
                           computer)))]
;     (take 2
           (calculate-output-for-state new-state))))
