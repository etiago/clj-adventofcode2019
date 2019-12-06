(ns clj-adventofcode2019.puzzle3
  (:gen-class)
  (:require [clojure.set :as set]))

(defn- load-data
  []
  (into [] (map (fn [line]
                  (map
                   (fn [part]
                     (let [trimmed (clojure.string/trim part)
                           direction (subs trimmed 0 1)
                           movement (Integer/parseInt (subs trimmed 1))]
                       (identity [direction movement])))
                   (clojure.string/split line #",")))
                (clojure.string/split-lines (slurp "resources/puzzle3.txt")))))

(defn- update-last-visited
  [last-visited movement-mask]
  (swap!
   last-visited
   (fn [prev]
     (into [] (map + prev movement-mask)))))

(defn- pure-update-last-visited
  [last-visited movement-mask]
  (into [] (map + last-visited movement-mask)))

(defn- direction-configs-cond
  [direction last-visited-x last-visited-y steps]
  (cond
    (= "R" direction) {:direction-fn #(identity [(+ % last-visited-x) last-visited-y])
                       :last-visited-mask [steps 0]
                       :cost-selector first}
    (= "L" direction) {:direction-fn #(identity [(- last-visited-x %) last-visited-y])
                       :last-visited-mask [(- steps) 0]
                       :cost-selector first}
    (= "D" direction) {:direction-fn #(identity [last-visited-x (- last-visited-y %)])
                       :last-visited-mask [0 (- steps)]
                       :cost-selector second}
    (= "U" direction) {:direction-fn #(identity [last-visited-x (+ last-visited-y %)])
                       :last-visited-mask [0 steps]
                       :cost-selector second}))

(defn- move
  [direction adding-range steps last-visited visited-set]
  (let [last-visited-x (first @last-visited)
        last-visited-y (second @last-visited)
        direction-configs (direction-configs-cond direction last-visited-x last-visited-y steps)
        direction-fn (:direction-fn direction-configs)
        last-visited-mask (:last-visited-mask direction-configs)
        new-positions (into #{} (map direction-fn adding-range))]
    (update-last-visited last-visited last-visited-mask)
    (swap! visited-set #(set/union % new-positions))))

(defn- visited-set-for-single-side
  [side]
  (let [last-visited (atom [1 1])
        visited-set (atom #{})]
    (dorun
     (map
      (fn [movement]
        (let [direction (first movement)
              steps (second movement)
              adding-range (range 1 (inc steps))]
          (move direction adding-range steps last-visited visited-set)))
      side))
    @visited-set))

(defn- manhattan-distance
  [origin point]
  (let [origin-x (first origin)
        origin-y (second origin)
        point-x (first point)
        point-y (second point)]
    (+ (Math/abs (- origin-x point-x)) (Math/abs (- origin-y point-y)))))

(defn run-pt1
  []
  (let [data (load-data)]
    (apply
     min
     (map
      #(manhattan-distance [1 1] %)
      (set/intersection (visited-set-for-single-side (first data)) (visited-set-for-single-side (second data)))))))

(defn- trace-generic
  [direction adding-range steps visited-set state]
  (let
      [last-visited-x (first (:last-visited state))
       last-visited-y (second (:last-visited state))
       direction-configs (direction-configs-cond direction last-visited-x last-visited-y steps)
       direction-fn (:direction-fn direction-configs)
       last-visited-mask (:last-visited-mask direction-configs)
       cost-selector (:cost-selector direction-configs)
       new-positions (into #{} (map direction-fn adding-range))
       matching-positions (set/intersection new-positions visited-set)]

    (assoc state
           :last-visited (pure-update-last-visited (:last-visited state) last-visited-mask)
           :running-cost (+ (:running-cost state) steps)
           :visited-map-to-cost (if (not-empty matching-positions)
                                  (apply merge (:visited-map-to-cost state) (map
                                                                             (fn [matching-pos]
                                                                               (let [cost (+ (:running-cost state) (Math/abs (- (cost-selector matching-pos) (cost-selector (:last-visited state)))))]
                                                                                 (identity {matching-pos cost})))
                                                                             matching-positions))
                                  (:visited-map-to-cost state)))))

(defn- trace
  [side visited-set state]
  (reduce
   (fn [old-state movement]
     (let [direction (first movement)
           steps (second movement)
           adding-range (range 1 (inc steps))
           new-state (trace-generic direction adding-range steps visited-set old-state)]
       new-state))
   state
   side))

(defn run-pt2
  []
  (let [data (load-data)
        matching-positions (set/intersection (visited-set-for-single-side (first data)) (visited-set-for-single-side (second data)))
        state {:last-visited [1 1]
               :running-cost 0
               :visited-map-to-cost {}}]
     (apply min (vals (merge-with +
                 (:visited-map-to-cost (trace (first data) matching-positions state))
                 (:visited-map-to-cost (trace (second data) matching-positions state)))))))
