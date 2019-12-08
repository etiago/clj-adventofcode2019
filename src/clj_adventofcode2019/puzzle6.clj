(ns clj-adventofcode2019.puzzle6
  (:gen-class)
  (:require [clojure.set :as set]))

(defn- load-orbits
  []
  (into [] (map #(clojure.string/split % #"\)")
                (clojure.string/split-lines (slurp "resources/puzzle6.txt")))))

(def total-map
  (atom {}))

(defn calculate-total
  [m orbits]
  (reduce #(let [prev (get %1 (first %2) 0)]
             (assoc %1 (second %2) (inc prev))) m orbits))

(defn run-pt1
  []
  (let [orbits (load-orbits)]
    (loop
        [m @total-map
         total 0]
      (let [new-total-map (calculate-total m orbits)
            new-total (reduce + (vals new-total-map))]
        (if (= total new-total)
          total
          (do
            (swap! total-map (fn [_] (identity new-total-map)))
            (recur @total-map new-total)))))))

(defn find-to-root
  [reversed-orbits p path-so-far]
  (if (contains? reversed-orbits p)
    (recur reversed-orbits (get reversed-orbits p) (clojure.set/union path-so-far #{(get reversed-orbits p)}))
    path-so-far))

(defn run-pt2
  []
  (let [orbits (load-orbits)
        reversed-orbits (reduce #(assoc %1 (second %2) (first %2)) {} orbits)
        you (find-to-root reversed-orbits "YOU" #{})
        san (find-to-root reversed-orbits "SAN" #{})]
    (count
     (clojure.set/union
      (clojure.set/difference you san)
      (clojure.set/difference san you)))))
