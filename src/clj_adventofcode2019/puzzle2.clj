(ns clj-adventofcode2019.puzzle2
  (:gen-class))

(defn- load-code
  []
  (into [] (map #(-> %
                     (clojure.string/trim)
                     (Integer/parseInt))
                (clojure.string/split (slurp "resources/puzzle2.txt") #","))))

(defn- operation
  [operation-fn first-value second-value result-idx program-code]
  (swap!
   program-code
   #(assoc
     %
     result-idx
     (operation-fn first-value second-value))))

(defn- addition
  [first-value second-value result-idx program-code]
  (operation + first-value second-value result-idx program-code))

(defn- multiplication
  [first-value second-value result-idx program-code]
  (operation * first-value second-value result-idx program-code))

(defn- extract-value
  [program-code running-idx]
  (nth @program-code (nth @program-code (+ 1 @running-idx))))

(defn- sum
  [a b c]
  (+ a b c))



(defn- calculate-output-for-noun-and-verb
  [noun verb]
  (let [program-code (atom (load-code))
        running-idx (atom 0)]
    (swap! program-code #(assoc % 1 noun 2 verb))
    (while (not= 99 (nth @program-code @running-idx))
      (let [operation (nth @program-code @running-idx)
            first-value (nth @program-code (nth @program-code (+ 1 @running-idx)))
            second-value (nth @program-code (nth @program-code (+ 2 @running-idx)))
            result-idx (nth @program-code (+ 3 @running-idx))

            ]
         (cond
           (= operation 1) (do
                             
                             (addition first-value second-value result-idx program-code)
                             (swap! running-idx #(+ % 4)))
           (= operation 2) (do
                             (multiplication first-value second-value result-idx program-code)
                             (swap! running-idx #(+ % 4))))))
           ;; (= operation 3) (do
           ;;                   (put-in-pos first-value program-code)
           ;;                   (swap! idx #(+ % 2))))))
    (first @program-code)))
  
(defn run-pt1
  []
  (prn (calculate-output-for-noun-and-verb 12 2)))

(defn run-pt2
  []
  (loop [noun 99 verb 99]
    (cond
      (= noun -1)(throw (Exception. "Did not find a solution."))
      (= 19690720 (calculate-output-for-noun-and-verb noun verb)) (identity {:noun noun :verb verb})
      (= verb 0) (recur (dec noun) 99)
      :else (recur noun (dec verb)))))
