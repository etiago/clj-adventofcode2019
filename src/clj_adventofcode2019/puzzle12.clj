(ns clj-adventofcode2019.puzzle12
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.core.async :refer [>! <! >!! <!! go chan buffer close! thread
                                        alts! alts!! timeout]]
            [clojure.math.combinatorics :as combo]))

(defn load-initial-positions
  []
  {:a {:x -2 :y 9 :z -5 :vel-x 0 :vel-y 0 :vel-z 0}
   :b {:x 16 :y 19 :z 9 :vel-x 0 :vel-y 0 :vel-z 0}
   :c {:x 0 :y 3 :z 6 :vel-x 0 :vel-y 0 :vel-z 0}
   :d {:x 11 :y 0 :z 11 :vel-x 0 :vel-y 0 :vel-z 0}})

(defn load-test-positions
  []
  (into {} (map-indexed
   (fn [idx planet]
     {idx
     (apply merge
            (map
             (fn [coord]
               (let [coord-parts (clojure.string/split coord #"=")]
                 {(keyword (first coord-parts)) (biginteger (second coord-parts)) :vel-x (biginteger 0) :vel-y (biginteger 0) :vel-z (biginteger 0)}))
             (clojure.string/split planet #", ")))})
   (-> (slurp "resources/puzzle12_test3.txt")
       (clojure.string/replace  "<" "")
       (clojure.string/replace  ">" "")
       (clojure.string/split-lines)))))

(defn- velocity-delta-for-position
  [first-position second-position]
  (cond
    (< first-position second-position) 1
    (> first-position second-position) -1
    (= first-position second-position) 0))

(defn- compute-new-velocities
  [keypair positions & optional-key-choice-pairs]
  (let [key-choice-pairs (if (empty? optional-key-choice-pairs) {:vel-x :x :vel-y :y :vel-z :z} (first optional-key-choice-pairs))
        first-key (first keypair)
        second-key (second keypair)
        first-positions (get positions first-key)
        second-positions (get positions second-key)]
    {first-key (merge first-positions
                      (into
                       {}
                       (map
                        #(let [velocity-key (first %)
                               coord-key (second %)]
                           {velocity-key (+ (biginteger (get first-positions velocity-key))
                                            (velocity-delta-for-position (get first-positions coord-key) (get second-positions coord-key)))})
                        key-choice-pairs)))
     second-key (merge second-positions
                      (into
                       {}
                       (map
                        #(let [velocity-key (first %)
                               coord-key (second %)]
                           {velocity-key (+ (biginteger (get second-positions velocity-key))
                                            (velocity-delta-for-position (get second-positions coord-key) (get first-positions coord-key)))})
                        key-choice-pairs)))}))

(defn- compute-new-positions
  [ks positions & optional-key-choice-pairs]
  (let [key-choice-pairs (if (empty? optional-key-choice-pairs) {:vel-x :x :vel-y :y :vel-z :z} (first optional-key-choice-pairs))]
    (map
     (fn [k]
       (identity
        {k (merge (get positions k)
                  (into {}
                        (map #(let [velocity-key (first %)
                                    coord-key (second %)]
                                {coord-key (+ (biginteger (get (get positions k) coord-key)) (get (get positions k) velocity-key))}) key-choice-pairs)))}))
     ks)))



(defn run-pt1
  []
  (let [itcount (atom 0)
        initial-positions (load-initial-positions)
        planet-pairs (into [] (combo/combinations (keys initial-positions) 2))
        iterations (take
                             (* 1000 (inc (count planet-pairs)))
                             ;(map
                             ; #(identity {:calculate-velocity %2 :keypair %1})
                              (cycle (conj planet-pairs (keys initial-positions))))]

    (let [final-state (reduce
                       #(if (= 2 (count %2))
                          (assoc %1 :data (merge (:data %1) (compute-new-velocities %2 (:data %1))))
                          (let [new-state (into {} (compute-new-positions %2 (:data %1)))
                                new-state-v (into [] new-state)]
                            (when (not (contains? (:visited %1) new-state-v))
                              (swap! itcount inc))
                            (assoc %1 :data (merge (:data %1) new-state) :visited (clojure.set/union (:visited %1) new-state-v))))
                       {:data initial-positions :visited #{}}
                       iterations)]
;      final-state
      (reduce
       +
       (map #(*
              (+ (.abs (.toBigInteger (:x %))) (.abs (.toBigInteger (:y %))) (.abs (.toBigInteger (:z %))))
              (+ (.abs (.toBigInteger (:vel-x %))) (.abs (.toBigInteger (:vel-y %))) (.abs (.toBigInteger (:vel-z %)))))  (vals (:data final-state))))
      )

    
      ;(reduce
      ; #()
      ; 0
      ; )
    ))

(defn- compute-state
  [state iterations ks]
  (if (= 2 (count (first iterations)))
    (let [new-velocities (compute-new-velocities (first iterations) (:data state) ks)
            new-data (merge (:data state) new-velocities)
            ;repeats-on-iterations (:repeats-on-iterations state)
            ;new-repeats-on-iterations (if (contains? (:visited state) new-data)
            ;                            (clojure.set/union repeats-on-iterations #{(:iteration state)}) repeats-on-iterations)
          new-state (assoc state
                           ;:repeats-on-iterations new-repeats-on-iterations
                           :data new-data)]

;        (println (str "new pos " new-state))
        (lazy-seq (compute-state new-state (rest iterations) ks)))
    (let [new-positions (into {} (compute-new-positions (first iterations) (:data state) ks))
          new-data (merge (:data state) new-positions)
          new-positions-v (into [] new-positions)
          repeats-on-iterations (:repeats-on-iterations state)
          new-repeats-on-iterations (if (contains? repeats-on-iterations (:data state))
                                      (assoc repeats-on-iterations (:data state) (conj (get repeats-on-iterations (:data state)) (:iteration state)))
                                      (assoc repeats-on-iterations (:data state) [(:iteration state)]))
          new-state (assoc state
                           :data new-data
                           :iteration (inc (:iteration state))
                           :repeats-on-iterations new-repeats-on-iterations
                           :visited (if (contains? (:visited state) (:data state))
                                      (merge (:visited state) {new-positions (clojure.set/union (get (:visited state) new-positions) #{(:iteration state)})})
                                      (merge (:visited state) {new-positions #{(:iteration state)}})))]


        ;(println (into [] new-positions))
        (lazy-seq (cons state (compute-state new-state (rest iterations) ks))))))
  
(defn run-pt2
  []
  (let [ks-x [:vel-x :x]
        ks-y [:vel-y :y]
        ks-z [:vel-z :z]
        initial-positions-x (into {} (map #(identity [(first %) (select-keys (second %) ks-x)]) (load-test-positions)))
        initial-positions-y (into {} (map #(identity [(first %) (select-keys (second %) ks-y)]) (load-test-positions)))
        initial-positions-z (into {} (map #(identity [(first %) (select-keys (second %) ks-z)]) (load-test-positions)))
        planet-pairs (into [] (combo/combinations (keys initial-positions-x) 2))
        iterations (cycle (conj planet-pairs (keys initial-positions-x)))]

;    (first (second (partition-by #(not (contains? (:visited %) (:data %)))
    (let [fx (future (last (take 3000 (compute-state {:data initial-positions-x :visited {} :iteration 0 :repeats-on-iterations {}} iterations (into {} [ks-x])))))
          fy (future (last (take 3000 (compute-state {:data initial-positions-y :visited {} :iteration 0 :repeats-on-iterations {}} iterations (into {} [ks-y])))))
          fz (future (last (take 3000 (compute-state {:data initial-positions-z :visited {} :iteration 0 :repeats-on-iterations {}} iterations (into {} [ks-z])))))
          repeats-x (apply clojure.set/union (vals (:visited @fx)))
          repeats-y (apply clojure.set/union (vals (:visited @fy)))
          repeats-z (apply clojure.set/union (vals (:visited @fz)))]

      (:repeats-on-iterations @fx)
      ;(:data (first (second (partition-by #(= (:iteration %) 769) (compute-state {:data initial-positions-x :visited {} :iteration 0 :repeats-on-iterations #{}} iterations (into {} [ks-x]))))))
      ;(:7402 repeats-x)
      ;;(doall (map
      ;; #(do
      ;;
      ;;    ;;(println (str "x " (get repeats-x %)))
      ;;    ;;(println (str "y " (get repeats-y %)))
      ;;    ;;(println (str "z " (get repeats-z %)))
      ;;    (= (get repeats-x %) (get repeats-y %) (get repeats-z %) ))
      ;; (clojure.set/intersection (into #{} (keys repeats-x)) (into #{} (keys repeats-y)) (into #{} (keys repeats-z)))))
      ;(clojure.set/subset? (clojure.set/subset? @fx @fy) @fz)


      )
    
      ;; (reduce
      ;;  +
      ;;  (map #(*
      ;;         (+ (.abs (.toBigInteger (:x %))) (.abs (.toBigInteger (:y %))) (.abs (.toBigInteger (:z %))))
      ;;         (+ (.abs (.toBigInteger (:vel-x %))) (.abs (.toBigInteger (:vel-y %))) (.abs (.toBigInteger (:vel-z %)))))  (vals (:data final-state))))
      ;; )

    
      ;(reduce
      ; #()
      ; 0
      ; )
  ))

