(ns clj-adventofcode2019.puzzle5
  (:gen-class)
  (:require [clojure.set :as set]))

(defn- extract-from-program-code
  [program-code running-idx cnt]
  (subvec @program-code (inc @running-idx) cnt))

(defn- load-code
  []
  (into [] (map #(-> %
                     (clojure.string/trim)
                     (Integer/parseInt))
                (clojure.string/split (slurp "resources/puzzle5.txt") #","))))

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
     (put-in-pos first-value (:program-code config) (:running-idx config) (:argcount config))))
  ([first-value program-code running-idx argcount]
   (let [input (Integer/valueOf (read-line))]
     (swap! program-code #(assoc % first-value input))
     (swap! running-idx #(+ % (inc argcount))))))

(defn- print-pos
  ([config]
   (fn [first-value]
     (print-pos first-value (:program-code config) (:running-idx config) (:argcount config))))
  ([first-value program-code running-idx argcount]
   (println first-value)
   (swap! running-idx #(+ % (inc argcount)))))

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
      :last-mask false
      ;:mask [true true false]
      }
   2 {:argcount 3
      :fn multiplication
      :last-mask false
      ;:mask [true true false]
      }
   3 {:argcount 1
      :fn put-in-pos
      :last-mask false
      ;:mask [false]
      }
   4 {:argcount 1
      :fn print-pos
      ;:last-mask true
      ;:mask [true]
      }
   5 {:argcount 2
      :fn jump-if-true
      ;:last-mask false
      }
   6 {:argcount 2
      :fn jump-if-false
      ;:last-mask false
      }
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
  ([]
   (calculate-output (atom (load-code))))
  ([program-code]
   (let [running-idx (atom 0)]
     (while (not= 99 (nth @program-code @running-idx))
       (let [opcode (nth @program-code @running-idx)
             operation-and-mask (extract-operation-and-mask opcode)
             operation (:operation operation-and-mask)
             mask (:mask operation-and-mask)
             operation-configs (get configs-per-operation operation)]
         (apply
          ((:fn operation-configs)
           {:program-code program-code
            :running-idx running-idx
            :argcount (:argcount operation-configs)})
          (extract-values-from-program-code
           program-code
           running-idx
           (:argcount operation-configs)
           mask)))))))
  
(defn run-pt1
  []
  ;(calculate-output (atom [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])))
  ;(calculate-output (atom [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 ])))
  (calculate-output))
